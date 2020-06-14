(ns serenity.operators
  (:require
   [lilactown.harmony :as harmony]
   [serenity.core :as s]))


(defn map [f & args])


(defn bind [f & args])


(defn collect
  ([f & args]
   (let [[xf inputs] (if (fn? (first args))
                       #js [(first args) (rest args)]
                       #js [nil args])
         rf (if (some? xf)
              (let [f' (xf f)]
                (fn [current inputs]
                  (apply f' current inputs)))
              (fn [current inputs]
                (apply f current inputs)))]
     (s/->Signal
      (harmony/ref nil)
      #(mapv deref inputs) ;; `input-fn`
      rf
      false
      (js/Set.)
      (js/Set.)
      1
      nil))))
