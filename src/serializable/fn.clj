(ns serializable.fn
  "Serializable functions! Check it out."
  (:refer-clojure :exclude [fn])
  (:import java.io.Writer))

(defn- save-env [bindings form]
  (let [form (with-meta (cons `fn (rest form)) ; serializable/fn, not core/fn
                        (meta form))
        quoted-form `(quote ~form)]
    (if bindings
      `(list `let [~@(for [b bindings,
                           let-arg [`(quote ~b)
                                    `(list `quote ~b)]]
                       let-arg)]
             ~quoted-form)
      quoted-form)))

(defmacro ^{:doc (str (:doc (meta #'clojure.core/fn))
                      "\n\n  Oh, but it also allows serialization!!!111eleven")}
  fn [& sigs]
  `(with-meta (clojure.core/fn ~@sigs)
     {:type ::serializable-fn
      ::source ~(save-env (keys &env) &form)}))

(defmethod print-method ::serializable-fn [o ^java.io.Writer w]
  (print-method (::source (meta o)) w))
