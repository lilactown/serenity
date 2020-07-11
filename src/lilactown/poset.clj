(ns lilactown.poset)


(def conj-set (fnil conj #{}))


(deftype TransientArrayPOSet [meta order-fn arr]
  Object
  (toString [coll]
    (pr-str coll))

  clojure.lang.ITransientCollection
  (conj [tc o]
    (let [order (order-fn o)]
      (aset arr order (conj-set (aget arr order) o)))
    tc)

  clojure.lang.ITransientSet
  (disjoin [tc o]
    (let [order (order-fn o)]
      (aset arr order (disj (aget arr order) o)))
    tc)

  clojure.lang.Counted
  (count [_]
    (reduce #(+ %1 (count %2)) 0 arr))

  clojure.lang.Seqable
  (seq [_]
    (apply concat arr))

  clojure.lang.ILookup
  (valAt [tc o]
    (.valAt tc o nil))
  (valAt [tc o nf]
    (get (aget arr (order-fn o))
            o nf))

  clojure.lang.IFn
  (invoke [tc k]
    (.valAt tc k nil))
  (invoke [tc k nf]
    (.valAt tc k nf)))


(defn poset
  [order-fn & els]
  (let [p (->TransientArrayPOSet nil order-fn (object-array 100))]
    (doseq [el els]
      (conj! p el))
    p))


(comment
  (let [ps (poset count [1] [2] [3 4] [5 6 7] [])]
    (doseq [x ps]
      (prn x))

    (conj! ps [2])
    (conj! ps [0 0])


    (doseq [x ps]
      (prn x))


    (prn (get ps [3 4]))

    (count ps)))
