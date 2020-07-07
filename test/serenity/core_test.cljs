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

(t/deftest deref-behavior
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         a (s/signal #(inc @src))]
     (t/is (= 0 @src))
     ;; `a` has not been connected yet, it is currently not calculating
     (t/is (= nil @a))

     (let [sink (s/sink a)]
       ;; `sink` connects immediately
       (t/is (= 1 @a))
       (t/is (= 1 @sink))

       (-> (awaitp #(some? @sink))
           (.then #(t/is (= 1 @sink)))
           (.then #(t/is (= 1 @a)))
           (.then #(s/send src 1))
           (.then (fn [] (awaitp #(= 2 @sink))))
           (.then #(t/is (= 1 @src)))
           (.then #(t/is (= 2 @sink)))
           (.then #(t/is (= 2 @a)))
           (.then done))))))


(t/deftest basic-graph
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         a (s/signal #(inc @src))

         b (s/signal #(inc @src))

         c (s/signal #(+ @a @b))

         values (atom [])

         sink (s/sink c)]

     (add-watch sink ::test (fn [_ _ _ n]
                              (swap! values conj n)))

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     (queue-send src 4)

     (-> (awaitp #(= @values [4 6 8 10]))
         (.then #(t/is (= @values [4 6 8 10])
                       "Expected values are equal"))
         (.then done)))))


(t/deftest connected?
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         a0 (s/signal #(deref src))
         a1 (s/signal #(deref a0))
         b (s/signal #(deref src))]
     (t/is (false? (s/connected? a0)))
     (t/is (false? (s/connected? a1)))
     (t/is (false? (s/connected? b)))

     ;; connections are scheduled for the end of this tick
     (let [sinkA (s/sink a1)]
       (t/is (true? (s/connected? src)))
       (t/is (false? (s/connected? b)))
       (t/is (true? (s/connected? a0)))
       (t/is (true? (s/connected? a1)))
       (t/is (true? (s/connected? sinkA)))

       (-> (awaitp #(s/connected? sinkA))
           (.then #(t/is (true? (s/connected? src))))
           (.then #(t/is (false? (s/connected? b) "b still unconnected")))
           (.then #(t/is (true? (s/connected? a1))))
           (.then #(t/is (true? (s/connected? a0))))
           (.then #(t/is (true? (s/connected? sinkA))))

           (.then #(s/dispose! sinkA))
           (.then #(t/is (false? (s/connected? src))))
           (.then #(t/is (false? (s/connected? b))))
           (.then #(t/is (false? (s/connected? a0))))
           (.then #(t/is (false? (s/connected? a1))))
           (.then #(t/is (false? (s/connected? sinkA))))

           (.then done))))))


(t/deftest deferred-connected?
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         a0 (s/signal #(deref src))
         a1 (s/signal #(deref a0))
         b (s/signal #(deref src))]
     (t/is (false? (s/connected? a0)))
     (t/is (false? (s/connected? a1)))
     (t/is (false? (s/connected? b)))

     ;; connections are scheduled for the end of this tick
     (let [sinkA (s/sink a1)]
       (t/is (true? (s/connected? src)))
       (t/is (false? (s/connected? b)))
       (t/is (true? (s/connected? a0)))
       (t/is (true? (s/connected? a1)))
       (t/is (true? (s/connected? sinkA)))

       (-> (awaitp #(s/connected? sinkA))
           (.then #(t/is (true? (s/connected? src))))
           (.then #(t/is (false? (s/connected? b) "b still unconnected")))
           (.then #(t/is (true? (s/connected? a1))))
           (.then #(t/is (true? (s/connected? a0))))
           (.then #(t/is (true? (s/connected? sinkA))))

           (.then #(s/dispose! sinkA))
           (.then #(t/is (false? (s/connected? src))))
           (.then #(t/is (false? (s/connected? b))))
           (.then #(t/is (false? (s/connected? a0))))
           (.then #(t/is (false? (s/connected? a1))))
           (.then #(t/is (false? (s/connected? sinkA))))

           (.then done))))))


(t/deftest simple-sync-disposal
  (t/testing "Synchronous sink create and then dispose does not trigger sink-f"
    (let [[src-calls src-f] (spy (fn [_ x] x))
          src (s/source src-f
                        :initial 0)

          [sink-calls sink-f] (spy #())
          sink (s/sink src)]

      (add-watch sink ::test sink-f)

      (t/is (= 0 @src-calls))

      (t/is (= 0 @sink-calls))

      (s/dispose! sink))))


(t/deftest sync-disposal
  (t/testing
    "That a sink which is disposed in the same tick as messages are sent will not run"
    (t/async
     done
     (let [src (s/source (fn [_ x] x)
                         :initial 0)
           values (atom [])
           [sink-calls sink-f] (spy (fn [_ _ n]
                                        (swap! values conj n)))
           sink (s/sink src)]

       (add-watch sink ::test sink-f)

       (s/send src 1)
       (s/send src 2)
       (s/dispose! sink)
       (s/send src 3)
       (s/send src 4)

       (-> (js/Promise.all
            #js [(awaitp #(= [] @values))
                 (awaitp #(= 0 @sink-calls))])
           (.then #(t/is (= [] @values)))
           (.then #(t/is (= 0 @sink-calls)))
           (.then done))))))


(t/deftest async-disposal
  (t/async
   done
   (let [src (s/source (fn [_ x] x)
                       :initial 0)
         values (atom [])
         [sink-calls sink-f] (spy (fn [_ _ _ n]
                                      (swap! values conj n)))
         sink (s/sink src)]
     (add-watch sink ::test sink-f)

     (queue-send src 1)
     (queue-send src 2)
     (queue #(s/dispose! sink))
     (queue-send src 3)
     (queue-send src 4)

     (-> (js/Promise.all
          #js [(awaitp #(= [1 2] @values))
               (awaitp #(= 2 @sink-calls))])
         (.then #(t/is (= [1 2] @values)))
         (.then #(t/is (= 2 @sink-calls)))
         (.then done)))))


(t/deftest complex-graph-disposal)


(t/deftest batching
  (t/testing "sending a group of messages are calculated separately (no batching)"
    (t/async
     done
     (let [src (s/source (fn [_ x] x)
                         :initial 0)
           values (atom [])
           [sink-calls sink-f] (spy (fn [_ _ _ n]
                                      (swap! values conj n)))

           sink (s/sink src)]
       (add-watch sink ::test sink-f)

       ;; send all in one batch
       (queue (fn []
                (s/send src 1)
                (s/send src 2)
                (s/send src 3)
                (s/send src 4)))
       (-> (js/Promise.all
            #js [(awaitp #(= [1 2 3 4] @values))
                 (awaitp #(= 4 @sink-calls))])
           (.then #(t/is (= [1 2 3 4] @values)))
           (.then #(t/is (= 4 @sink-calls)))
           (.then done))))))


(t/deftest memoizing-outputs
  (t/async
   done
   (let [[src-calls src-f] (spy (fn [_ _]
                                  {:asdf "jkl"}))
         src (s/source src-f :initial {:asdf "jkl"})
         a (s/signal #(deref src))
         b (s/signal #(+ @a))
         [sink-calls sink-f] (spy #())
         sink (s/sink b)]
     (add-watch sink ::test sink-f)

     (queue-send src nil)
     (queue-send src nil)
     (queue-send src nil)

     (-> (awaitp #(= 4 @src-calls))
         (.then #(t/is (= 3 @src-calls)))
         (.then #(t/is (= 0 @sink-calls)))
         (.then done)))))


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
           c (s/signal c-f)

           sink (s/sink c)]

       (s/send src 1)

       (await #(= [2 2 2 2]
                  [@a0-calls @a-calls @b-calls @c-calls])
              "Expected calls match"
              done)))))


(t/deftest multiple-sinks
  (t/async
   done
   (let [[src-calls src-f] (spy (fn [_ x] x))
         src (s/source src-f
                       :initial 0)

         [s0-calls s0-f] (spy #())
         s0 (s/sink src)

         [s1-calls s1-f] (spy #())
         s1 (s/sink src)]
     (add-watch s0 ::test s0-f)
     (add-watch s1 ::test s1-f)

     (t/is (= 0 @src-calls))

     (queue-send src 1)
     (queue-send src 2)
     (queue-send src 3)
     ;; dispose s1
     (queue #(s/dispose! s1))
     (queue-send src 4)

     (-> (awaitp #(= 4 @src-calls))
         (.then #(t/is (= 4 @src-calls)))
         (.then #(t/is (= 4 @s0-calls)))
         (.then #(t/is (= 3 @s1-calls)))
         (.then done)))))


(t/deftest simple-transducer)


(t/deftest filter-transducer)


(t/deftest stateful-transducer)


(t/deftest early-termination)


(t/deftest on-connect)


(t/deftest cycles)


(t/deftest error
  (t/async
   done
   (let [src (s/source (fn [_ x] x) :initial 0)

         sA (s/signal #(inc @src))

         sB (s/signal #(if (= @src 3)
                         (throw (js/Error "No 3s allowed"))
                         (+ 100 @src)))

         sC (s/signal #(+ @sA @sB))

         sink (s/sink sC)]
     s/send src 3)
     (queue #(do (t/is (= 0 @src))
                 (t/is (= 1 @sA))
                 (t/is (= 100 @sB))
                 (t/is (= 101 @sC))
                 (done))))))


(t/deftest batched-error)


(t/deftest subscription-like)


(t/deftest async-values)


#_(t/run-tests)
