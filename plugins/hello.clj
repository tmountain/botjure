(ns plugins.hello)
(declare hello)

(defn hello [arg]
  {:payload (str "Hello, " (:from arg) "!") :to (:to arg)})

(defn thank [arg]
  {:payload (str "You are most welcome, " (:from arg) "!") :to (:to arg)})

(defn echo [arg]
  {:payload (:msg arg), :to (:to arg)})

(defn dance [arg]
  {:payload (vector ":D-<" ":D|-<" ":D/-<") :to (:to arg)})

(defn hello-dispatch [arg]
  (let [cmd (:cmd arg)]
    (cond (= cmd "hello") (hello arg)
          (= cmd "thanks") (thank arg)
          (= cmd "echo") (echo arg)
          (= cmd "dance") (dance arg)
          :else {})))

(def properties {:name      "hello",
                 :matches   :all,
                 :dispatch  hello-dispatch })
