(ns plugins.hello)
(declare hello)

(defn hello [arg]
  {:payload (str "Hello " (:from arg) "!") :to (:to arg)})

(def properties {:name      "hello",
                 :matches   ["hello"],
                 :dispatch  hello })
