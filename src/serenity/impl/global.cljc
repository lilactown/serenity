(ns serenity.impl.global)


(def ^:dynamic *reactive-context* nil)


(def effect-queue (atom []))


(defn defer
  [f]
  (swap! effect-queue conj f))


(def mailbox (atom []))


(defn add-message
  [src msg]
  (swap! mailbox conj [src msg]))
