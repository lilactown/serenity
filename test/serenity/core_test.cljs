(ns serenity.core-test
  (:require
   [cljs.test :as t :include-macros true]
   [serenity.core :as s]
   [clojure.string]))


;;
;; Utils
;;

(defn await
  ([pred desc]
   (await pred desc #()))
  ([pred desc done]
   (letfn [(cb [current-tries]
             (cond
               (or (pred) (zero? current-tries))
               (do (t/is (pred) desc)
                   (done))

               :else (js/setTimeout
                      #(cb (dec current-tries))
                      100)))]
     (cb 10))
   nil))


(defn awaitp
  [pred]
  (js/Promise.
   (fn [resolve reject]
     (letfn [(cb [current-tries]
               (cond
                 (pred)
                 (resolve)

                 (zero? current-tries)
                 (resolve)

                 :else (js/setTimeout
                        #(cb (dec current-tries))
                        100)))]
       (cb 10)))))


(defn queue
  [f]
  (js/setTimeout f 0))


(defn queue-send
  [src msg]
  (queue #(s/send src msg)))


(defn spy
  [f]
  (let [calls (atom 0)]
    [calls (fn [& args]
             (swap! calls inc)
             (apply f args))]))


;;
;; Tests
;;


(t/deftest basic-graph
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         a (s/signal #(inc @src))

         b (s/signal #(inc @src))

         c (s/signal #(+ @a @b))

         values (atom [])]

     (s/sink! c (fn [_ _ n]
                 (swap! values conj n)))

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     (queue-send src 4)

     (-> (awaitp #(= @values [2 4 6 8 10]))
         (.then #(t/is (= @values [2 4 6 8 10])
                       "Expected values are equal"))
         (.then done)))))


(t/deftest simple-sync-disposal
  (t/testing
    "That a sink which is disposed in the same tick as messages are sent will not run"
    (t/async
     done
     (let [src (s/source (fn [_ x] x)
                         :initial 0)
           values (atom [])
           [sink!-calls sink!-f] (spy (fn [_ _ n]
                                        (swap! values conj n)))
           sink! (s/sink! src sink!-f)]

       (s/send src 1)
       (s/send src 2)
       (s/dispose! sink!)
       (s/send src 3)
       (s/send src 4)

       (-> (js/Promise.all
            #js [(awaitp #(= [0] @values))
                 (awaitp #(= 1 @sink!-calls))])
           (.then #(t/is (= [0] @values)))
           ;; `1` because it ran once on initial state
           (.then #(t/is (= 1 @sink!-calls)))
           (.then done))))))


(t/deftest simple-async-disposal
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         values (atom [])
         [sink!-calls sink!-f] (spy (fn [_ _ n]
                                    (swap! values conj n)))
         sink! (s/sink! src sink!-f)]

     (queue-send src 1)
     (queue-send src 2)
     (queue #(s/dispose! sink!))
     (queue-send src 3)
     (queue-send src 4)

     (-> (js/Promise.all
          #js [(awaitp #(= [0 1 2] @values))
               (awaitp #(= 3 @sink!-calls))])
         (.then #(t/is (= [0 1 2] @values)))
         ;; `3` because it ran once on initial state
         (.then #(t/is (= 3 @sink!-calls)))
         (.then done)))))


(t/deftest complex-graph-disposal)


(t/deftest batching
  (t/async
    done
    (let [src (s/source (fn [_ x] x)
                        :initial 0)
          values (atom [])
          [sink!-calls sink!-f] (spy (fn [_ _ n]
                                     (swap! values conj n)))]
      (s/sink! src sink!-f)
      ;; send all in one batch
      (queue (fn []
               (s/send src 1)
               (s/send src 2)
               (s/send src 3)
               (s/send src 4)))
      (-> (js/Promise.all
           #js [(awaitp #(= [0 4] @values))
                (awaitp #(= 2 @sink!-calls))])
          (.then #(t/is (= [0 4] @values)))
          ;; `2` because it ran once on initial state
          (.then #(t/is (= 2 @sink!-calls)))
          (.then done)))))


(t/deftest memoizing-outputs)


(t/deftest nasty-diamond
  (t/testing "That each node is only fired once in the graph"
    (t/async
     done
     (let [src (s/source (fn [_ x] x)
                         :initial 0)

           [a0-calls a0-f] (spy #(inc @src))
           a0 (s/signal a0-f)


           [a-calls a-f] (spy #(inc @a0))
           a (s/signal a-f)

           [b-calls b-f] (spy #(dec @src))
           b (s/signal b-f)

           [c-calls c-f] (spy #(+ @a @b))
           c (s/signal c-f)]

       ;; make it happen
       (s/sink! c (fn [_ _ _]))

       (t/is (= [1 1 1 1]
                [@a0-calls @a-calls @b-calls @c-calls]))

       (s/send src 1)

       (await #(= [2 2 2 2]
                  [@a0-calls @a-calls @b-calls @c-calls])
              "Expected calls match"
              done)))))


(t/deftest simple-transducer)


(t/deftest filter-transducer)


(t/deftest stateful-transducer)


(t/deftest simple-collect
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                        :initial 0)

         history (s/collect (fn [log x]
                              (conj log x))
                            src)]
     ;; make it hot
     (s/sink! history #())

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     (queue-send src 4)

     (-> (awaitp #(= '(4 3 2 1 0) @history))
         (.then #(t/is (= '(4 3 2 1 0) @history)))
         (.then done)))))


(t/deftest collect-with-filter-transducer
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)

         history (s/collect (fn [log x]
                              (conj log x))
                            (filter even?)
                            src)]
     ;; make it hot
     (s/sink! history #())

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     (queue-send src 4)

     (-> (awaitp #(= '(4 2 0) @history))
         (.then #(t/is (= '(4 2 0) @history)))
         (.then done)))))


(t/deftest collect-with-map-transducer
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)

         history (s/collect (fn [log x]
                              (conj log x))
                            (map #(* 2 %))
                            src)]
     ;; make it hot
     (s/sink! history #())

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     (queue-send src 4)

     (-> (awaitp #(= '(8 6 4 2 0) @history))
         (.then #(t/is (= '(8 6 4 2 0) @history)))
         (.then done)))))


(t/deftest collect-with-dropping-transducer
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)

         history (s/collect (fn [log x]
                              (conj log x))
                            (comp (drop 3)
                                  (map #(* 2 %)))
                            src)]
     ;; make it hot
     (s/sink! history #())

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     (queue-send src 4)

     (-> (awaitp #(= '(8 6) @history))
         (.then #(t/is (= '(8 6) @history)))
         (.then done)))))


(t/deftest collect-with-multiple-inputs
  (t/async
   done
   (let [srcA (s/source (fn [_ x] x)
                        :initial 0)

         srcB (s/source (fn [_ x] x)
                        :initial "a")

         history (s/collect (fn [log x y]
                              (conj log [x y]))
                            srcA srcB)]
     ;; make it hot
     (s/sink! history #())

     (queue-send srcA 1)
     (queue-send srcB "b")
     (queue-send srcA 2)
     (queue-send srcB "c")
     (queue-send srcA 3)
     (queue-send srcB "d")
     (queue-send srcA 4)
     (queue-send srcB "e")

     (-> (awaitp #(= '([4 "e"]
                       [3 "d"]
                       [2 "c"]
                       [2 "b"]
                       [1 "a"]
                       [0 "a"])
                     @history))
         (.then #(t/is (= '([4 "e"]
                            [3 "d"]
                            [2 "c"]
                            [1 "b"]
                            [0 "a"])
                          @history)))
         (.then done)))))


(t/deftest collect-with-multiple-inputs-transducer
  (t/async
   done
   (let [srcA (s/source (fn [_ x] x)
                        :initial 0)

         srcB (s/source (fn [_ x] x)
                        :initial "a")

         history (s/collect (fn [log [x y]]
                              (conj log [x y]))
                            (map #(vector
                                   (inc %1)
                                   (clojure.string/upper-case %2)))
                            srcA srcB)]
     ;; make it hot
     (s/sink! history #())

     (queue-send srcA 1)
     (queue-send srcB "b")
     (queue-send srcA 2)
     (queue-send srcB "c")
     (queue-send srcA 3)
     (queue-send srcB "d")
     (queue-send srcA 4)
     (queue-send srcB "e")

     (-> (awaitp #(= '([5 "E"]
                       [4 "D"]
                       [4 "C"]
                       [3 "B"]
                       [2 "B"]
                       [1 "A"])
                     @history))
         (.then #(t/is (= '([5 "E"]
                            [4 "D"]
                            [4 "C"]
                            [3 "B"]
                            [2 "B"]
                            [1 "A"])
                          @history)))
         (.then done)))))


(t/deftest on-connect)


(t/deftest cycles)


#_(t/run-tests)
