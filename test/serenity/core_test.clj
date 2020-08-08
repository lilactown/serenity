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

    (s/stabilize! src)

    (t/is (= 2 @C-runs))
    (t/is (= {:A 3 :B 2} @snk))

    (s/dispose! snk)))


(t/deftest connect-dispose
  (let [src (s/source 0)
        sig (s/signal #(inc @src))]

    (t/is (= 0 @src))
    (t/is (= ::s/init @sig))
    (t/is (not (s/connected? src)))
    (t/is (not (s/connected? sig)))

    (s/send src 1)
    (s/stabilize! src)

    (t/is (= 1 @src))
    (t/is (= ::s/init @sig))

    ;; connect w/ a sink
    (let [snk (s/sink sig)]
      (t/is (= 2 @sig))
      (t/is (s/connected? src))
      (t/is (s/connected? sig))
      (t/is (s/connected? snk))

      (s/send src 2)
      (s/stabilize! src)

      (t/is (= 3 @sig))

      (s/dispose! snk)

      (t/is (not (s/connected? src)))
      (t/is (not (s/connected? sig)))
      (t/is (not (s/connected? snk)))
      (t/is (= 3 @sig))

      (s/send src 3)
      (s/stabilize! src)

      ;; no change
      (t/is (= 3 @sig)))))


(t/deftest watch
  (let [src (s/source 0)
        sig (s/signal #(inc @src))
        snk (s/sink sig)

        calls (atom 0)]
    (add-watch snk :call (fn [_ _ _ _]
                           (swap! calls inc)))

    (s/send src 1)
    (s/stabilize! src)

    (t/is (= 1 @calls))))


(t/deftest errors
  (let [src (s/source 0)
        sigA0 (s/signal #(inc @src))
        sigA1 (s/signal #(if (= 2 @sigA0)
                          (throw (ex-info "oh no" {}))
                          "ok"))
        sigB (s/signal #(* 2 @src))
        sigC (s/signal (fn []
                         {:A @sigA1
                          :B @sigB}))
        snk (s/sink sigC)
        calls (atom 0)]

    (add-watch snk :call (fn [_ _ _ _]
                           (swap! calls inc)))

    (s/stabilize! src)
    (t/is (= 1 @sigA0))
    (t/is (= 0 @sigB))


    (s/send src 1)
    (s/send src 2)
    (s/send src 3)
    (try
      (s/stabilize! src)
      (catch Exception e
        nil))

    (t/is (= 1 @sigA0))
    (t/is (= 0 @sigB))
    (t/is (= 0 @calls))))

(t/deftest disconnect-unused
  (let [src (s/source 0)
        calls (atom {:A 0 :B 0 :C 0})
        sigA (s/signal #(do
                          (swap! calls update :A inc)
                          (inc @src)))
        sigB (s/signal #(do
                          (swap! calls update :B inc)
                          (- -1 @src)))
        sigC (s/signal #(do
                          (swap! calls update :C inc)
                          (if (and (< 0 @src) (> 4 @src))
                            @sigB
                            @sigA)))
        snk (s/sink sigC)]

    (s/stabilize! src)
    (t/is (s/connected? src))
    (t/is (s/connected? sigA))
    (t/is (not (s/connected? sigB)))
    (t/is (s/connected? sigC))
    (t/is (= {:A 1
              :B 0
              :C 1} @calls))

    (s/send src 1)
    (s/stabilize! src)
    (t/is (s/connected? src))
    (t/is (not (s/connected? sigA)))
    (t/is (s/connected? sigB))
    (t/is (s/connected? sigC))
    ;; A gets called once more (since it was previously connected)
    (t/is (= {:A 2
              :B 1
              :C 2} @calls))

    (s/send src 2)
    (s/stabilize! src)
    (t/is (s/connected? src))
    (t/is (not (s/connected? sigA)))
    (t/is (s/connected? sigB))
    (t/is (s/connected? sigC))
    ;; A now disconnected and not called
    (t/is (= {:A 2
              :B 2
              :C 3} @calls))

    (s/send src 3)
    (s/stabilize! src)
    (t/is (s/connected? src))
    (t/is (not (s/connected? sigA)))
    (t/is (s/connected? sigB))
    (t/is (s/connected? sigC))
    (t/is (= {:A 2
              :B 3
              :C 4} @calls))))


(t/deftest multiple-sources
  (let [src0 (s/source "0")
        srcA (s/source "A")
        sig (s/signal #(str @src0 @srcA))
        snk (s/sink sig)]
    (s/stabilize! src0 srcA)
    (t/is (= "0A" @snk))

    (s/send src0 "1")
    (s/send srcA "B")

    ;; stabilize only srcA
    (s/stabilize! srcA)
    (t/is (= "0B" @snk))
    (s/stabilize! src0)
    (t/is (= "1B" @snk))))
