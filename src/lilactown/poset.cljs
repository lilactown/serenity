(ns lilactown.poset)


(def conj-set (fnil conj #{}))


(deftype TransientArrayPOSet [meta order-fn arr]
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (pr-sequential-writer
     writer
     pr-writer
     "#poset [" " " "]"
     opts arr))

  Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))

  (keys [coll]
    ;; TODO
    )
  (entries [coll]
    ;; TODO
    )
  (values [coll]
    ;; TODO
    )
  (has [coll k]
    ;; TODO
    )
  (forEach [coll f]
    ;; TODO
    )

  IIterable
  (-iterator [coll]
    ;; TODO
    )

  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? meta new-meta)
      coll
      (TransientArrayPOSet. new-meta order-fn arr)))

  ITransientCollection
  (-conj! [tc o]
    (let [order (order-fn o)]
      (aset arr order (conj-set (aget arr order) o)))
    tc)

  ITransientSet
  (-disjoin! [tc o]
    (let [order (order-fn o)]
      (aset arr order (disj (aget arr order) o)))
    tc)

  ICounted
  (-count [_]
    (.reduce arr #(+ %1 (count %2)) 0))

  ISeqable
  (-seq [_]
    (apply concat arr))

  ILookup
  (-lookup [tc o]
    (-lookup tc o nil))
  (-lookup [tc o nf]
    (-lookup (aget arr (order-fn o)) o nf))

  IFn
  (-invoke [tc k]
    (-lookup tc k))
  (-invoke [tc k nf]
    (-lookup tc k nf)))


(defn poset
  [order-fn & els]
  (let [p (TransientArrayPOSet. nil order-fn #js [])]
    (doseq [el els]
      (conj! p el))
    p))
