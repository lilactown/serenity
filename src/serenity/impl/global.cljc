(ns serenity.impl.global)


(def ^:dynamic *reactive-context* nil)


(def effect-queue (atom {}))


(defn defer
  [f & args]
  (swap! effect-queue assoc f args))


(defn clear-effects
  []
  (reset! effect-queue {}))


(def mailbox (atom []))


(defn add-message!
  [src msg]
  (swap! mailbox conj [src msg]))


(defn set-mailbox!
  [new]
  (reset! mailbox new))
