(ns melody.core
  (:require [lilactown.harmony :as harmony]
            [lilactown.dynamic-scope :as ds]))


(extend-protocol IPrintWithWriter
  js/Set
  (-pr-writer [this writer opts]
    (pr-sequential-writer
     writer pr-writer "#object[Set #{" " " "}]"
     opts this)))


(defprotocol ISource
  (-send [src x]))


(defprotocol IReactive
  (-add-edge [a b])
  (-remove-edge [a b]))


(defprotocol INode
  (-order [node])
  (-set-order [node n])
  (-calculate [node]))


(def ^:dynamic *reactive-context*)


(def ^:dynamic *transaction*)


(defn- ordered-set
  [coll]
  (apply sorted-set-by (comp < #(-order %)) coll))


(defn- calculate-all-nodes!
  [initial-edges]
  (loop [edges (ordered-set initial-edges)
         n 10]
    (when-some [edge (first edges)]
      (when (> n 0)
        (prn edge edges)
        (recur (into (disj edges edge)
                     (-calculate edge))
               (dec n))))))


(deftype Source [ref reducer edges]
  IDeref
  (-deref [this]
    (when (some? *reactive-context*)
      ;; `js/Set`
      (.add *reactive-context* this))
    (harmony/deref ref))

  ISource
  (-send [_ x]
    (doto (harmony/branch)
      ;; calculate new state of source
      (.add #(harmony/alter
              ref
              (fn [current]
                (reducer current x))))
      ;; calculate nodes
      (.add #(calculate-all-nodes! edges))
      (.commit))
    nil)

  IReactive
  (-add-edge [_ node]
    (.add edges node))
  (-remove-edge [_ node]
    (.delete edges node))

  INode
  (-order [_] 0)
  ;; TODO these aren't used
  (-set-order [_ _] 0)
  (-calculate [_] nil))


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


;; `edges` are things that depend on me
(deftype Node [ref f xf edges ^:mutable derefs ^:mutable order]
  IDeref
  (-deref [this]
    (when (some? *reactive-context*)
      (.add *reactive-context* this))
    (harmony/deref ref))

  INode
  (-order [_] order)
  (-set-order [_ n]
    (when (> n order)
      (set! order n)))
  (-calculate [this]
    (let [derefs' (js/Set.)]
      ;; run `f` with `*reactive-context*` set so that we can diff our derefs
      ;; and clean up any that have become stale
      (binding [*reactive-context* derefs']
        ;; TODO run `xf` for transducing
        (harmony/set ref (f)))

      ;;
      ;; TODO this all needs to _after_ the transaction has committed.
      ;;

      ;; remove ourself from derefs that are stale
      (doseq [deref (set-difference derefs derefs')]
        (-remove-edge deref this))

      ;; TODO only do this for difference the other way maybe?
      (doseq [deref derefs']
        (-add-edge deref this))

      ;; set current derefs
      (set! derefs derefs')


      ;; TODO set order

      ;; return edges to be calculated
      edges)))


(defn source
  [reducer default]
  (->Source (harmony/ref default) reducer (js/Set.)))


(defn node
  [f]
  (let [node (->Node
              (harmony/ref nil)
              f
              nil ;; `xf`
              (js/Set.) ;; `edges`
              (js/Set.) ;; `derefs`
              ;; assume at least order 11
              1)]
    ;; run first calculation eagerly
    (-> (harmony/branch)
        (.add #(-calculate node))
        (.commit))
    node))


(defn send [src message]
  (-send src message))


(comment
  (def a (source #(case %2
                    :inc (inc %1)
                    :dec (dec %1)) 0))

  (def b (node #(inc @a)))

  (def c (node #(+ @a @b)))

  (send a :dec)

  @a

  @b
)
