(ns plugins.hello)
(declare hello)

(defn hello [arg]
  {:payload (str "Hello, " (:from arg) "!") :to (:to arg)})

(defn thank [arg]
  {:payload (str "You are most welcome, " (:from arg) "!") :to (:to arg)})

(defn echo [arg]
  {:payload (:msg arg) :to (:to arg)})

(defn dance [arg]
  {:payload (vector ":D-<" ":D|-<" ":D/-<") :to (:to arg)})

(defn hello-dispatch [arg]
  (loop [matches (:matches arg)]
    (if (not (= (count matches) 0))
      (let [m (first matches)]
        (if (coll? m)
          (recur m)
          (cond
            (= m "hello") (hello arg)
            (= m "dance") (dance arg)
            (= m "echo") (echo arg)
            (= m "ank") (thank arg)
            :else (recur (rest matches))))))))

(def properties {:name      "hello",
                 :matches   ["hello" "dance" "echo" #"th(ank)"],
                 :dispatch  hello-dispatch })
