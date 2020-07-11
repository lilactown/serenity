(ns serenity.protocols)

(defprotocol ISource
  (-receive [src x]))


(defprotocol IConnect
  (-connect [node])
  (-connected? [node]))


(defprotocol IReactive
  (-calculate [node]))


(defprotocol IOrdered
  (-set-order [node n])
  (-order [node]))


(defprotocol INode
  (-add-edge [a b])
  (-remove-edge [a b]))


(defprotocol ISink
  (-dispose [sink]))
