(ns melody.core
  (:require [lilactown.harmony :as harmony]
            [clojure.set :as set]))


(defprotocol IAgent
  (-send [agent f]))


(defprotocol IReactive
  (-add-dep [this r])
  (-remove-dep [this r]))


(defprotocol ITrack
  (-trigger [this]))


(def ^:dynamic *reactive-context* nil)

(def ^:dynamic *current-tracker* nil)


(deftype Agent [ref ^:mutable dependents]
  IAgent
  (-send [this f]
    (doto (harmony/branch)
      (.add #(harmony/alter ref f))
      ;; TODO schedule
      (.add #(doseq [dep dependents]
               (-trigger dep)))
      (.flush)
      (.commit)))

  IReactive
  (-remove-dep [this r]
    (.filter dependents #(not= r %)))

  IDeref
  (-deref [this]
    (when *reactive-context*
      (.push dependents *current-tracker*)
      (.push *reactive-context* this))
    (harmony/deref ref)))


(deftype Tracker [f ^:mutable ref dependents ^:mutable depends-on]
  ITrack
  (-trigger [this]
    ;; assume we're in a transaction
    (binding [*reactive-context* #js []
              *current-tracker* this]
      (harmony/alter ref f)
      ;; TODO defer setting of depends-on until after
      (let [depends-on' (set *reactive-context*)]
        (doseq [dep (set/difference depends-on depends-on')]
          (prn "n")
          (-remove-dep dep this))
        (set! depends-on depends-on')))
    (doseq [dep dependents]
      ;; TODO schedule
      (-trigger dep)))

  IReactive
  (-add-dep [this r]
    (.push depends-on r))

  IDeref
  (-deref [this]
    (when *reactive-context*
      ;; defer this
      (.push dependents *current-tracker*))
    (if (some? ref)
      (harmony/deref ref)
      ;; initialize
      (binding [*reactive-context* #js []
                *current-tracker* this]
        (set! ref (harmony/ref (f)))
        (set! depends-on (set *reactive-context*))
        (harmony/deref ref)))))


(defn agent
  [x]
  (->Agent (harmony/ref x) #js []))


(defn send
  [agent f]
  (-send agent f)
  nil)


(defn track*
  [f]
  (->Tracker f nil #js [] #js []))


(comment
  (def foo (agent 0))


  @foo


  (def bar (track* #(deref foo)))


  @bar

  (.-dependents foo)

  (.-depends-on bar)

  (send foo inc)
  )
