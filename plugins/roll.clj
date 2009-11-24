(ns plugins.roll)
(use '[clojure.contrib.str-utils :only (str-join)])
(declare roll)


(defn parse-integer [str]
  (try (Integer/parseInt str)
       (catch NumberFormatException nfe 0)))

(defn parse-roll [msg]
  (let [[_ sides count] (.split msg " ")]
    [(parse-integer sides) (parse-integer count)]))

(defn dice-roll [sides]
  (inc (rand-int sides)))

(defn roll [arg]
  (let [[sides count] (parse-roll (:msg arg))
        payload (for [x (range count)] (dice-roll sides))]
        {:payload (str "[" (str-join ", " payload) "]") :to (:to arg)}))

(def properties {:name      "roll",
                 :matches   ["roll"],
                 :dispatch  roll })
