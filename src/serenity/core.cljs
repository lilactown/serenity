(ns serenity.core
  (:require
   [lilactown.harmony :as harmony]
   [lilactown.poset :refer [poset]]))


(comment
  (extend-protocol IPrintWithWriter
    js/Set
    (-pr-writer [this writer opts]
      (pr-sequential-writer
       writer pr-writer "#object[Set [" " " "]]"
       opts this))))


(defprotocol ISource
  (-receive [src x]))


(defprotocol IReactive
  (-calculate [node])
  (-connect [node])
  (-connected? [node]))


(defprotocol IOrdered
  (-set-order [node n])
  (-order [node]))


(defprotocol INode
  (-add-edge [a b])
  (-remove-edge [a b]))


(defprotocol ISink
  (-dispose [sink]))


(def ^:dynamic *reactive-context*)


(def ^:dynamic *transaction*)


(def connect-queue #js [])


(def message-queue #js [])


(def stabilize-this-tick? false)

;;
;; The general approach we use for optimally calculating nodes is a topological
;; sort, where each node's height is: max(height of nodes I depend on) + 1
;;
;; This is implemented across the code; first here, where we sort by the
;; `-order` method return value, and `-set-order`, which ensures that `-order`
;; is always monotonically increasing for a given node, and `-calculate` which
;; handles calling `-set-order` after each calculation.
(defn- calculate-all-nodes!
  [initial-nodes]
  (loop [nodes (apply poset -order initial-nodes)
         ;; TODO remove governor
         n 100]
    (when-some [node (first nodes)]
      (when (> n 0)
        (when (< n 80)
          (prn :runaway))
        (doseq [node' (-calculate node)]
          (conj! nodes node'))
        (disj! nodes node)
        (recur nodes
               (dec n))))))


(defn- connect-sinks!
  [sinks]
  (doseq [sink sinks]
    (-connect sink)))


(defn- stabilize!
  [connections messages]
  (-> (harmony/branch)
      ;; TODO rewrite this to add thunk for each calculation
      (.add #(connect-sinks! connections))
      (.add #(calculate-all-nodes!
              (.reduce messages
                       (fn [edges [src message]]
                         (into edges (-receive src message)))
                       #{})))
      (.commit)))


(defn schedule-stabilize!
  []
  (when-not stabilize-this-tick?
    (set! stabilize-this-tick? true)
    (js/queueMicrotask
     (fn []
       (try
         (stabilize! connect-queue message-queue)
         (catch js/Object e
           (js/setTimeout #(throw e) 0))
         (finally
           (set! message-queue #js [])
           (set! connect-queue #js [])
           (set! stabilize-this-tick? false)))))))


(deftype Source [ref reducer edges meta]
  IMeta
  (-meta [_]
    meta)

  IDeref
  (-deref [this]
    (when (some? *reactive-context*)
      ;; `js/Set`
      (.add ^js *reactive-context* this))
    (harmony/deref ref))

  ISource
  (-receive [_ x]
    (harmony/alter
     ref
     (fn [current]
       (reducer current x)))
    edges)

  IOrdered
  (-order [_] 0)
  (-set-order [_ _] 0)

  INode
  (-add-edge [_ node]
    (.add ^js edges node))
  (-remove-edge [_ node]
    (.delete ^js edges node)))


(defn- set-difference
  [s1 s2]
  (->> s1
       (filter #(not (.has s2 %)))
       (js/Set.)))


(defn- set-intersection
  [s1 s2]
  (-> s1
      (filter #(.has s2 %))
      (js/Set.)))


(defn- set-union
  [s1 s2]
  (js/Set.
   (concat
    (js/Set. #js [1 2 3])
    (js/Set. #js [2 3 4]))))


;; signals are lazy!
(deftype Signal [state input-fn rf xf
                 ^:mutable f
                 ^:mutable connected?
                 ^:mutable edges-to-me
                 ^:mutable edges-from-me-to-other
                 ^:mutable order
                 meta]
  IMeta
  (-meta [_]
    meta)

  IDeref
  (-deref [this]
    (when (some? *reactive-context*)
      (.add ^js *reactive-context* this)
      (when-not connected?
        ;; calculate now since we're being lazy
        (-connect this)))
    (let [v (harmony/deref state)]
      (if (reduced? v)
        @v
        v)))

  INode
  (-add-edge [_ node]
    (.add ^js edges-to-me node))
  (-remove-edge [this node]
    (.delete ^js edges-to-me node)
    (when (zero? (.-size ^js edges-to-me))
      (set! connected? false)
      (set! f nil)
      ;; not listened to by anyone, remove it from the graph
      (doseq [node edges-from-me-to-other]
        (-remove-edge node this))))

  IOrdered
  (-order [_] order)
  (-set-order [_ n]
    (when (> n order)
      (set! order n)))

  IReactive
  (-connected? [_]
    connected?)
  (-connect [this]
    (set! connected? true)
    (if (some? xf)
      (set! f (xf rf))
      (set! f rf))
    (-calculate this))
  (-calculate [this]
    (let [edges-from-me-to-other' (js/Set.)
          old (harmony/deref state)]
      ;; run `f` with `*reactive-context*` set so that we can diff our edges-from-me-to-other
      ;; and clean up any that have become stale
      (binding [*reactive-context* edges-from-me-to-other']
        (harmony/alter
         state
         f
         (input-fn)))

      ;;
      ;; TODO this all needs to _after_ the transaction has committed.
      ;;

      (set! connected? true)
      (if (reduced? (harmony/deref state))
        ;; if reduced, disconnect ourself from listening to any changes
        (do
          ;; remove ourself from all edges-from-me-to-other
          (doseq [node (set-union edges-from-me-to-other' edges-from-me-to-other)]
            (-remove-edge node this))

          ;; clear out to potentially help w/ GC
          (set! edges-from-me-to-other nil))

        (do
          ;; remove ourself from edges-from-me-to-other that are stale
          (doseq [node (set-difference edges-from-me-to-other edges-from-me-to-other')]
            (-remove-edge node this))

          ;; expectation is that adding an edge is idempotent
          (doseq [node edges-from-me-to-other']
            (-add-edge node this)
            ;; set the order of this node to be at least as big as it's biggest edge
            ;; to enable topological sorting when calculating
            (-set-order this (inc (-order node))))

          ;; set current edges-from-me-to-other
          (set! edges-from-me-to-other edges-from-me-to-other')))
      ;; return edges to be calculated
      (when-not (= old (harmony/deref state))
        edges-to-me))))


;; sinks are eager!
(deftype Sink [state
               ^:mutable connected?
               ^:mutable disposed?
               ^:mutable node
               ^:mutable watches
               ^:mutable order
               meta]
  IDeref
  (-deref [_]
    (when *reactive-context*
      (throw (ex-info "Cannot deref sink inside of a node" {})))
    (harmony/deref state))

  IWatchable
  ;; (-notify-watches [this old new])
  (-add-watch [this key watch-fn]
    (set! (.-watches this) (assoc watches key watch-fn)))
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key)))

  ISink
  (-dispose [this]
    ;; try to allow this sink and any of it's dependencies to be GCd
    (-remove-edge node this)
    (set! node nil)
    (set! watches nil)
    (set! connected? false)
    (set! disposed? true))

  IOrdered
  (-order [_] order)
  (-set-order [_ n]
    (when (> n order)
      (set! order n)))

  IReactive
  (-connected? [_]
    connected?)
  (-connect [this]
    (when-not disposed?
      (set! connected? true)
      (-calculate this)))
  (-calculate [this]
    (when connected?
      (let [old (harmony/deref state)]
        ;; by dereferencing the node here, we will implicitly initialize their
        ;; calculation and any child node's calculations as well
        (binding [*reactive-context* (js/Set.)]
          (harmony/set state @node))

        (-add-edge node this)
        (-set-order this (inc (-order node)))

        ;; TODO run watch after commit
        (doseq [[k f] watches]
          (f k this old (harmony/deref state)))

        ;; sinks never have edges-to-me
        nil))))


(defn source
  "Creates a new source node. Takes a 2-arity reducer function of current state,
  and a message, which returns the next state.

  `deref` / `@` returns the current state. When dereferenced inside of a signal,
  will trigger a re-calculation of that signal on change.

  `send` can be used to send messages to the source in order to trigger a
  change.

  `:initial` is an optional keyword arg for the initial state. Default `nil`."
  [reducer & {:keys [initial]}]
  (->Source (harmony/ref initial) reducer (js/Set.) nil))


(defn signal
  "Creates a new signal node. Takes a function which dereferences one or more
  other signals or sources and returns a value.

  `deref` / `@` returns the current state. When dereferenced inside of a signal,
  will trigger a re-calculation of that signal on change.

  By default, signals do not compute their values unless they are connected to
  a signal which is listened to by a sink.

  Optionally, you may pass a reducing function `rf` as the first argument which
  takes the current state and the next computed value by `input-fn` to return
  the new state of the signal."
  ([input-fn] (signal (fn [_ input] input) nil input-fn))
  ([rf input-fn] (signal rf nil input-fn))
  ([rf xf input-fn]
   (->Signal
    (harmony/ref nil)
    input-fn
    rf
    xf
    nil ;; `f`
    false ;; `connected?`
    (js/Set.) ;; `edges-from-me-to-other`
    (js/Set.) ;; `edges-to-me`
    ;; assume at least order 1
    1
    ;; meta
    nil)))


(defn sink
  "Creates a new sink node. Sinks listen to signals or sources and run
  `on-change` when a new value is available.

  Sinks will immediately connect and calculate all nodes in the transitive
  graph.

  To destroy and disconnect listening, use `dispose!` on the sink."
  [input & {:keys [defer-connect?]
            :or {defer-connect? true}}]
  (let [s (->Sink
           (harmony/ref nil)
           false ;; `disposed?`
           false ;; `connected?`
           input ;; `node`
           {} ;; `watches`
           1 ;; default order
           ;; meta
           nil)]
    ;; connect up to the graph
    (if defer-connect?
      (do
        (.push connect-queue s)
        (schedule-stabilize!))
      ;; synchronously stabilize!
      (do (stabilize! #js [s] #js [])))
    s))


(defn dispose!
  "Disposes a sink. Stops listening to changes and disconnects it from the
  graph.

  If a signal no longer has any sinks listening to it in its transitive
  dependents, it will also disconnect from the graph and not recalculate on
  changes to its dependencies."
  [sink]
  (-dispose sink))


(defn connected?
  "Returns whether a sink or signal is actively connected to a source and
  receiving updates."
  [node]
  (-connected? node))


(defn send
  "Sends a new message to the source asynchronously. Returns the source."
  [src message]
  (.push message-queue #js [src message])
  (schedule-stabilize!)
  src)
