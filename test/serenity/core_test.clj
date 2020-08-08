(ns serenity.core-test
  (:require
   [clojure.test :as t]
   [serenity.core :as s]))


(t/deftest nasty-diamond
  (let [src (s/source 0)

        ;; branch A
        sigA1 (s/signal #(inc @src))
        sigA2 (s/signal #(inc @sigA1))

        ;; branch B
        sigB (s/signal #(* 2 @src))

        ;; join back together
        C-runs (atom 0)
        sigC (s/signal (fn []
                         ;; record how many times sigC is run
                         (swap! C-runs inc)
                         {:A @sigA2
                          :B @sigB}))

        snk (s/sink sigC)]

    (t/is (= 1 @C-runs))
    (t/is (= {:A 2 :B 0} @snk))

    (s/send src 1)

    ;; hasn't been run yet
    (t/is (= 1 @C-runs))
    (t/is (= {:A 2 :B 0} @snk))

    (s/stabilize!)

    (t/is (= 2 @C-runs))
    (t/is (= {:A 3 :B 2} @snk))

    (s/dispose! snk)))
