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
  (-calculate [node]))

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


(def message-queue #js [])

;;
;; The general approach we use for optimally calculating nodes is a topological
;; sort, where each node's height is: max(height of nodes I depend on) + 1
;;
;; This is implemented across the code piece; first here, where we sort by the
;; `-order` method return value, and `-set-order`, which ensures that `-order`
;; is always monotonically increasing for a given node, and `-calculate` which
;; handles calling `-set-order` after each calculation.
;;


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
(deftype Signal [state input-fn rf
                 ^:mutable initialized?
                 ^:mutable to-edges
                 ^:mutable from-edges
                 ^:mutable order
                 meta]
  IMeta
  (-meta [_]
    meta)

  IDeref
  (-deref [this]
    (if (some? *reactive-context*)
      (do (.add ^js *reactive-context* this)
          (when-not initialized?
            ;; calculate now since we're being lazy
            (-calculate this)))
      (when-not initialized?
        ;; for REPLing, create branch and calculate
        ;; TODO figure out a way not to set the value of state?
        (-> (harmony/branch)
            (.add #(-calculate this))
            (.commit))))
    (let [v (harmony/deref state)]
      (if (reduced? v)
        @v
        v)))

  INode
  (-add-edge [_ node]
    (.add ^js to-edges node))
  (-remove-edge [this node]
    (.delete ^js to-edges node)
    (when (zero? (.-size ^js to-edges))
      (set! initialized? false)
      ;; disposing. remove it from the graph
      (doseq [node from-edges]
        (-remove-edge node this))))

  IOrdered
  (-order [_] order)
  (-set-order [_ n]
    (when (> n order)
      (set! order n)))

  IReactive
  (-calculate [this]
    (let [from-edges' (js/Set.)
          old (harmony/deref state)]
      ;; TODO check if different before propagating

      ;; run `f` with `*reactive-context*` set so that we can diff our from-edges
      ;; and clean up any that have become stale
      (binding [*reactive-context* from-edges']
        (harmony/alter
         state
         rf
         (input-fn)))

      ;;
      ;; TODO this all needs to _after_ the transaction has committed.
      ;;

      (set! initialized? true)
      (when (some? *reactive-context*)
        (if (reduced? (harmony/deref state))
          ;; if reduced, disconnect ourself from listening to any changes
          (do
            ;; remove ourself from all from-edges
            (doseq [node (set-union from-edges' from-edges)]
              (-remove-edge node this))

            ;; clear out to potentially help w/ GC
            (set! from-edges nil))

          (do
            ;; remove ourself from from-edges that are stale
            (doseq [node (set-difference from-edges from-edges')]
              (-remove-edge node this))

            ;; expectation is that adding an edge is idempotent
            (doseq [node from-edges']
              (-add-edge node this)
              ;; set the order of this node to be at least as big as it's biggest edge
              ;; to enable topological sorting when calculating
              (-set-order this (inc (-order node))))

            ;; set current from-edges
            (set! from-edges from-edges'))))
      ;; return edges to be calculated
      (when-not (identical? old (harmony/deref state))
        to-edges))))


;; sinks are eager!
(deftype Sink [state ^:mutable node ^:mutable watch ^:mutable order meta]
  IDeref
  (-deref [_]
    (when *reactive-context*
      (throw (ex-info "Cannot deref sink inside of a node" {})))
    (harmony/deref state))

  ISink
  (-dispose [this]
    ;; try to allow this sink and any of it's dependencies to be GCd
    (-remove-edge node this)
    (set! node nil)
    (set! watch nil))

  IOrdered
  (-order [_] order)
  (-set-order [_ n]
    (when (> n order)
      (set! order n)))

  IReactive
  (-calculate [this]
    (let [old (harmony/deref state)]
      ;; do this only because otherwise we'll start a new branch
      (binding [*reactive-context* (js/Set.)]
        (harmony/set state @node))

      (-add-edge node this)
      (-set-order this (inc (-order node)))

      ;; TODO run watch after commit
      (watch this old (harmony/deref state))

      ;; sinks never have to-edges
      nil)))


(defn source
  [reducer default]
  (->Source (harmony/ref default) reducer (js/Set.) nil))


(defn signal
  ([input-fn] (signal input-fn nil))
  ([input-fn xf]
   (let [rf (fn [_ input] ;; reducer function just takes the input
              input)
         node (->Signal
               (harmony/ref nil)
               input-fn
               ;; `rf`
               (if (some? xf)
                 (xf rf)
                 rf)
               false ;; `initialized?`
               (js/Set.) ;; `from-edges`
               (js/Set.) ;; `to-edges`
               ;; assume at least order 1
               1
               ;; meta
               nil)]
     node)))


(defn sink
  [input on-change]
  (let [s (->Sink
           (harmony/ref nil)
           input
           on-change
           1 ;; default order
           ;; meta
           nil)]
    ;; run sink eagerly
    (-> (harmony/branch)
        (.add #(-calculate s))
        (.commit))
    s))


(defn- stabilize!
  []
  (try
    (-> (harmony/branch)
        ;; TODO rewrite this to add thunk for each calculation
        (.add #(calculate-all-nodes!
                (.reduce message-queue
                         (fn [edges [src message]]
                           (into edges (-receive src message)))
                         #{})))
        (.commit))
    (catch js/Object e
      (js/setTimeout #(throw e) 0))
    (finally
      (set! message-queue #js []))))


(defn send [src message]
  (.push message-queue #js [src message])
  (when (= 1 (.-length message-queue))
    (js/queueMicrotask stabilize!))
  src)


(comment
  (def a (source #(case %2
                    :inc (inc %1)
                    :dec (dec %1)) 0))

  (def b (signal #(do (prn 'b) (inc @a))))

  (def c (signal #(do (prn 'c) (inc @a))))

  (def d (signal #(do (prn 'd) @b)
                 (take 5)))

  (def e (signal #(do (prn 'e) (+ @c @d))))

  (def s (sink e (fn [_ o n] (prn o n))))

  (-order a)

  (-order b)

  (-order c)

  (do (send a :inc))

  @a

  @b

  @c

  @d

  @e

  (-dispose s)
)
