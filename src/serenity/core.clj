(ns serenity.core
  (:require
   [clojure.set :as s]
   [lilactown.poset :refer (poset)]
   [serenity.protocols :as sp])
  (:refer-clojure :exclude (send)))


(def ^:dynamic *reactive-context* nil)


(deftype Source [state reducer connected? edges]
  clojure.lang.IDeref
  (deref [this]
    (when (some? *reactive-context*)
      (conj! *reactive-context* this)
      (when-not @connected?
        (sp/-connect this)))
    @state)

  sp/ISource
  (-receive [_ x]
    (alter state reducer x)
    @edges)

  sp/IConnect
  (-connected? [_]
    @connected?)
  (-connect [_]
    (ref-set connected? true))

  sp/IOrdered
  (-order [_] 0)
  (-set-order [_ _] 0)

  sp/INode
  (-add-edge [_ node]
    (alter edges conj node)
    (ref-set connected? true)
    nil)
  (-remove-edge [_ node]
    (alter edges disj node)

    (when (zero? (count @edges))
      (ref-set connected? false))

    nil))


(deftype Signal [state input-fn reducer connected?
                 edges-to-me edges-from-me-to-other
                 order]
  clojure.lang.IDeref
  (deref [this]
    (when (some? *reactive-context*)
      (conj! *reactive-context* this)
      (when-not @connected?
        (sp/-connect this)))
    (let [current @state]
      (if (reduced? current)
        @current
        current)))

  sp/INode
  (-add-edge [_ node]
    (alter edges-to-me conj node))
  (-remove-edge [this node]
    (alter edges-to-me disj node)

    (when (zero? (count @edges-to-me))
      (ref-set connected? false)
      (doseq [node @edges-from-me-to-other]
        (sp/-remove-edge node this))))

  sp/IOrdered
  (-order [_] @order)
  (-set-order [_ n]
    (when (> n @order)
      (ref-set order n)))

  sp/IConnect
  (-connected? [_]
    @connected?)
  (-connect [this]
    (prn :connect)
    (ref-set connected? true)
    (sp/-calculate this))

  sp/IReactive
  (-calculate [this]
    (let [edges-from-me-to-other' (transient #{})
          current @state]
      (binding [*reactive-context* edges-from-me-to-other']
        (alter state reducer (input-fn)))

      (ref-set connected? true)

      (let [edges-from-me-to-other' (persistent! edges-from-me-to-other')]
        (prn :edges-from-me-to-other' edges-from-me-to-other')
        (if (reduced? @state)
          (do
            (doseq [node (s/union edges-from-me-to-other'
                                  @edges-from-me-to-other)]
              (sp/-remove-edge node this))
            (ref-set edges-from-me-to-other nil))

          (do
            (doseq [node (s/difference @edges-from-me-to-other
                                       edges-from-me-to-other')]
              (sp/-remove-edge node this))

            (doseq [node edges-from-me-to-other']
              (sp/-add-edge node this)
              (sp/-set-order this (inc (sp/-order node))))))

        (ref-set edges-from-me-to-other edges-from-me-to-other'))

      (when-not (= current @state)
        @edges-to-me))))


(deftype Sink [state ;; ref of T
               connected? ;; ref of bool
               disposed? ;; ref of bool
               node ;; signal
               watches ;; atom of map
               order;; ref of int
               ]
  clojure.lang.IDeref
  (deref [_]
    (when (some? *reactive-context*)
      (throw (ex-info "Cannot reference sink inside of signal/sink")))
    @state)

  clojure.lang.IRef
  ;; TODO do I need setValidator / getValidator?
  (addWatch [_ k f]
    (swap! watches assoc k f)
    nil)
  (removeWatch [_ k]
    (swap! watches dissoc k)
    nil)
  (getWatches [_]
    @watches)

  sp/ISink
  (-dispose [this]
    (sp/-remove-edge node this)
    (dosync
     (ref-set node nil)
     (ref-set connected? false)
     (ref-set disposed? true))
    (reset! watches {}))

  sp/IOrdered
  (-order [_] @order)
  (-set-order [_ n]
    (when (> n @order)
      (ref-set order n)))

  sp/IConnect
  (-connected? [_]
    @connected?)
  (-connect [this]
    (when-not @disposed?
      (ref-set connected? true)
      (sp/-calculate this)
      nil))

  sp/IReactive
  (-calculate [this]
    (when connected?
      (let [old @state]
        ;; we ignore the resulting reactive-context since we know we only depend
        ;; on this one node
        (binding [*reactive-context* (transient #{})]
          (ref-set state @node))

        (sp/-add-edge node this)
        (sp/-set-order this (inc (sp/-order node)))

        (doseq [[k f] @watches]
          (f k this old @state))

        nil))))


(defn source [reducer & {:keys [initial]}]
  (->Source
   ;; state
   (ref initial)
   ;; reducer
   reducer
   ;; connected?
   (ref false)
   ;; edges
   (ref #{})))


(defn signal
  ([input-fn]
   (signal (fn [_ x] x) input-fn))
  ([reducer input-fn]
   (->Signal
    ;;state
    (ref ::init)
    ;; input-fn
    input-fn
    ;; reducer
    reducer
    ;; connected?
    (ref false)
    ;; edges-to-me
    (ref #{})
    ;; edges-from-me-to-other
    (ref #{})
    ;; order
    (ref 1))))


(defn sink
  [node]
  (doto (->Sink
         ;; state
         (ref ::init)
         ;; connected?
         (ref false)
         ;; disposed?
         (ref false)
         ;; node
         node
         ;; watches
         (atom {})
         ;; order
         (ref 1))
    (-> (sp/-connect)
        (dosync))))


(def ^:private mailbox (atom []))


(defn send [src message]
  (swap! mailbox conj [src message]))


(defn stabilize! []
  (doseq [[src msg] @mailbox]
    (dosync
     (loop [nodes (apply poset sp/-order (doto (sp/-receive src msg)
                                           (->> (prn :receive))))
            ;; TODO remove governor
            n 100]
       (when-some [node (first nodes)]
         (when (> n 0)
           (when (< n 80)
             (prn :runaway))
           (doseq [node' (sp/-calculate node)]
             (conj! nodes node'))
           (disj! nodes node)
           (recur nodes
                  (dec n)))))))
  (reset! mailbox []))


(comment
(def src (source (fn [_ x] x) :initial 0))

@src

@(.-edges src)

(def a (signal #(* @src 2)))

@a

(def snk (sink a))


(add-watch snk :prn (fn [_ _ _ x] (prn :prn x)))


(remove-watch snk :prn)


(send src 4)

(stabilize!)
)
