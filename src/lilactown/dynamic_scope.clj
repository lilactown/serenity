(ns lilactown.dynamic-scope)

(defmacro with-scope
  [scope & body]
  `(in-scope
    ~scope
    (fn ~'scope-body []
      ~@body)))
