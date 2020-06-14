(ns serenity.operators_test
  (:require
   [cljs.test :as t :include-macros true]
   [serenity.operators :as so]))

;; (t/deftest simple-collect
;;   (t/async
;;    done
;;    (let [src (s/source (fn [_ x] x)
;;                        :initial 0)

;;          history (s/collect (fn [log x]
;;                               (conj log x))
;;                             src)]
;;      ;; make it hot
;;      (s/sink! history #())

;;      (queue-send src 1)
;;      (queue-send src 2)
;;      (queue-send src 3)
;;      (queue-send src 4)

;;      (-> (awaitp #(= '(4 3 2 1 0) @history))
;;          (.then #(t/is (= '(4 3 2 1 0) @history)))
;;          (.then done)))))


;; (t/deftest collect-with-filter-transducer
;;   (t/async
;;    done
;;    (let [src (s/source (fn [_ x] x)
;;                        :initial 0)

;;          history (s/collect (fn [log x]
;;                               (conj log x))
;;                             (filter even?)
;;                             src)]
;;      ;; make it hot
;;      (s/sink! history #())

;;      (queue-send src 1)
;;      (queue-send src 2)
;;      (queue-send src 3)
;;      (queue-send src 4)

;;      (-> (awaitp #(= '(4 2 0) @history))
;;          (.then #(t/is (= '(4 2 0) @history)))
;;          (.then done)))))


;; (t/deftest collect-with-map-transducer
;;   (t/async
;;    done
;;    (let [src (s/source (fn [_ x] x)
;;                        :initial 0)

;;          history (s/collect (fn [log x]
;;                               (conj log x))
;;                             (map #(* 2 %))
;;                             src)]
;;      ;; make it hot
;;      (s/sink! history #())

;;      (queue-send src 1)
;;      (queue-send src 2)
;;      (queue-send src 3)
;;      (queue-send src 4)

;;      (-> (awaitp #(= '(8 6 4 2 0) @history))
;;          (.then #(t/is (= '(8 6 4 2 0) @history)))
;;          (.then done)))))


;; (t/deftest collect-with-dropping-transducer
;;   (t/async
;;    done
;;    (let [src (s/source (fn [_ x] x)
;;                        :initial 0)

;;          history (s/collect (fn [log x]
;;                               (conj log x))
;;                             (comp (drop 3)
;;                                   (map #(* 2 %)))
;;                             src)]
;;      ;; make it hot
;;      (s/sink! history #())

;;      (queue-send src 1)
;;      (queue-send src 2)
;;      (queue-send src 3)
;;      (queue-send src 4)

;;      (-> (awaitp #(= '(8 6) @history))
;;          (.then #(t/is (= '(8 6) @history)))
;;          (.then done)))))


;; (t/deftest collect-with-multiple-inputs
;;   (t/async
;;    done
;;    (let [srcA (s/source (fn [_ x] x)
;;                         :initial 0)

;;          srcB (s/source (fn [_ x] x)
;;                         :initial "a")

;;          history (s/collect (fn [log x y]
;;                               (conj log [x y]))
;;                             srcA srcB)]
;;      ;; make it hot
;;      (s/sink! history #())

;;      (queue-send srcA 1)
;;      (queue-send srcB "b")
;;      (queue-send srcA 2)
;;      (queue-send srcB "c")
;;      (queue-send srcA 3)
;;      (queue-send srcB "d")
;;      (queue-send srcA 4)
;;      (queue-send srcB "e")

;;      (-> (awaitp #(= '([4 "e"]
;;                        [3 "d"]
;;                        [2 "c"]
;;                        [2 "b"]
;;                        [1 "a"]
;;                        [0 "a"])
;;                      @history))
;;          (.then #(t/is (= '([4 "e"]
;;                             [3 "d"]
;;                             [2 "c"]
;;                             [1 "b"]
;;                             [0 "a"])
;;                           @history)))
;;          (.then done)))))


;; (t/deftest collect-with-multiple-inputs-transducer
;;   (t/async
;;    done
;;    (let [srcA (s/source (fn [_ x] x)
;;                         :initial 0)

;;          srcB (s/source (fn [_ x] x)
;;                         :initial "a")

;;          history (s/collect (fn [log [x y]]
;;                               (conj log [x y]))
;;                             (map #(vector
;;                                    (inc %1)
;;                                    (clojure.string/upper-case %2)))
;;                             srcA srcB)]
;;      ;; make it hot
;;      (s/sink! history #())

;;      (queue-send srcA 1)
;;      (queue-send srcB "b")
;;      (queue-send srcA 2)
;;      (queue-send srcB "c")
;;      (queue-send srcA 3)
;;      (queue-send srcB "d")
;;      (queue-send srcA 4)
;;      (queue-send srcB "e")

;;      (-> (awaitp #(= '([5 "E"]
;;                        [4 "D"]
;;                        [4 "C"]
;;                        [3 "B"]
;;                        [2 "B"]
;;                        [1 "A"])
;;                      @history))
;;          (.then #(t/is (= '([5 "E"]
;;                             [4 "D"]
;;                             [4 "C"]
;;                             [3 "B"]
;;                             [2 "B"]
;;                             [1 "A"])
;;                           @history)))
;;          (.then done)))))

