(ns plugins.roll)
(use '[clojure.contrib.str-utils :only (str-join)])
(declare roll)


(defn parse-integer [str]
  (try (Integer/parseInt str)
       (catch NumberFormatException nfe 0)))

(defn parse-roll [msg]
  (let [[roll sides count] (.split msg " ")]
    [(parse-integer sides) (parse-integer count)]))

(defn dice-roll [sides]
  (+ (rand-int sides) 1))

(defn roll [arg]
  (let [[sides count] (parse-roll (:msg arg))]
    (loop [count count
           payload []]
      (if (= count 0)
        {:payload (str "[" (str-join ", " payload) "]") :to (:to arg)}
        (recur (- count 1) (conj payload (dice-roll sides)))))))
(def properties {:name      "roll",
                 :matches   ["@roll"],
                 :dispatch  roll })
