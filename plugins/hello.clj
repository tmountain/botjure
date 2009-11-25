(ns plugins.hello)
(declare hello)

(defn hello [arg]
  {:payload (str "Hello, " (:from arg) "!") :to (:to arg)})

(defn thank [arg]
  {:payload (str "You are most welcome, " (:from arg) "!") :to (:to arg)})

(defn hello-dispatch [arg]
  (let [cmd (:cmd arg)]
    (cond (re-find #"hello.*" cmd) (hello arg)
          (re-find #"thank.*" cmd) (thank arg)
          :else {})))

(def properties {:name      "hello",
                 :matches   :all,
                 :dispatch  hello-dispatch })
