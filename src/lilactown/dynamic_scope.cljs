(ns lilactown.dynamic-scope
  (:require-macros [lilactown.dynamic-scope]))


(def ^:dynamic *scope*)


(defprotocol IScoped
  (-set! [ref x]))


;;
;; The approach taken here is to store each scope's value on the ref,
;; rather than store each ref's value in some kind of scope object.
;;
;; The rationale is mainly for GC purposes: if a `ScopedRef` could be
;; collected, but a global scope object was keeping it alive, we would
;; need to build some sort of ref counting or other mechanism to clear
;; the references to refs so they could be GC'd.
;;
;; Doing it this way, a scoped ref can be collected as soon as possible
;; like any other normal object. The thing that might be kept alive is
;; the `scope` object, but this is expected to live fairly long and since
;; the implementation only requires it to be a value, it can be very cheap.
;;

(deftype ScopedRef [default scope->value]
  IDeref
  (-deref [this]
    (let [x (.get scope->value *scope*)]
      (if (undefined? x)
        default
        x)))

  IScoped
  (-set! [this x]
    (.set scope->value *scope* x)
    x))


(defn create
  []
  (gensym "scope"))


(defn ref
  [default]
  (->ScopedRef default (js/Map.)))


(defn in-scope
  [scope f]
  (binding [*scope* scope]
    (f)))


(defn scoped-set!
  [ref x]
  (-set! ref x))


(comment
  (def r (scoped-ref "asdf"))


  (scoped-set! r "jkl")

  @r


  (in-scope
   (scope)
   #(deref r))
  )
