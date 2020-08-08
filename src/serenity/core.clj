(ns serenity.core
  (:require
   [clojure.set :as s]
   [lilactown.poset :refer (poset)]
   [serenity.impl.global :as global]
   [serenity.protocols :as sp])
  (:refer-clojure :exclude (send)))


(deftype Source [state reducer connected? edges on-connect on-disconnect]
  clojure.lang.IDeref
  (deref [this]
    (when (some? global/*reactive-context*)
      (conj! global/*reactive-context* this)
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
  (-connect [this]
    (ref-set connected? true)
    (when (some? on-connect)
      (on-connect this)))

  sp/IOrdered
  (-order [_] 0)
  (-set-order [_ _] 0)

  sp/INode
  (-add-edge [_ node]
    (alter edges conj node)
    (ref-set connected? true)
    nil)
  (-remove-edge [this node]
    (alter edges disj node)

    (when (zero? (count @edges))
      (ref-set connected? false)
      (when (some? on-disconnect)
        (on-disconnect this)))

    nil))


(deftype Signal [state input-fn reducer connected?
                 edges-to-me edges-from-me-to-other
                 order]
  clojure.lang.IDeref
  (deref [this]
    (when (some? global/*reactive-context*)
      (conj! global/*reactive-context* this)
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
    (ref-set connected? true)
    (sp/-calculate this))

  sp/IReactive
  (-calculate [this]
    (let [edges-from-me-to-other' (transient #{})
          current @state]
      (binding [global/*reactive-context* edges-from-me-to-other']
        (alter state reducer (input-fn)))

      (ref-set connected? true)

      (let [edges-from-me-to-other' (persistent! edges-from-me-to-other')]
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
               node ;; ref of signal of T
               watches ;; atom of map
               order;; ref of int
               on-dispose]
  clojure.lang.IDeref
  (deref [_]
    (when (some? global/*reactive-context*)
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
    (when (some? on-dispose)
      (on-dispose this))
    (dosync
     (sp/-remove-edge @node this)
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
      (let [old @state
            node @node]
        ;; we ignore the resulting reactive-context since we know we only depend
        ;; on this one node
        (binding [global/*reactive-context* (transient #{})]
          (ref-set state @node))

        (sp/-add-edge node this)
        (sp/-set-order this (inc (sp/-order node)))

        (doseq [[k f] @watches]
          (global/defer f k this old @state))

        nil))))


(defn source
  ([initial]
   (source (fn [_ x] x) initial))
  ([reducer initial & {:keys [on-connect on-disconnect]}]
   (->Source
    ;; state
    (ref initial)
    ;; reducer
    reducer
    ;; connected?
    (ref false)
    ;; edges
    (ref #{})
    ;; on-connect
    on-connect
    ;; on-disconnect
    on-disconnect)))


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
  [node & {:keys [on-dispose]}]
  (doto (->Sink
         ;; state
         (ref ::init)
         ;; connected?
         (ref false)
         ;; disposed?
         (ref false)
         ;; node
         (ref node)
         ;; watches
         (atom {})
         ;; order
         (ref 1)
         on-dispose)
    (-> (sp/-connect)
        (dosync))))


(defn send [src message]
  (global/add-message! src message))


(defn dispose! [sink]
  (sp/-dispose sink))


(defn connected? [node]
  (sp/-connected? node))


(defn stabilize! [& sources]
  (let [err (atom nil)
        sources (set sources)
        {messages true
         leftover false} (group-by #(boolean (some sources %)) @global/mailbox)]
    (try
      (dosync
       (doseq [[src msg] messages]
         (loop [nodes (apply poset sp/-order (sp/-receive src msg))
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
                      (dec n))))))
       (doseq [[f args] @global/effect-queue]
         (apply f args)))
      (catch Throwable e
        (reset! err e))
      (finally
        (global/set-mailbox! (or leftover []))
        (global/clear-effects)
        (when-some [e @err]
          (throw e))))))
