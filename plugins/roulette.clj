(ns plugins.roulette)
(declare roulette)

(def score (ref {}))

(defn roulette [arg]
  (let [r (inc (rand-int 3))
        to (:to arg)]
    (cond
      (= r 1) {:to to
               :payload (str "CLICK! " (:from arg) " got lucky.")}
      (= r 2) (do
                (if (contains? @score (:from arg))
                  (dosync (alter score assoc (:from arg) (inc ((:from arg) @score))))
                  (dosync (alter score assoc (:from arg) 1)))
                {:to to
                 :payload (str "BANG! " (:bot-name arg) " watches " (:from arg) "'s brain splatter all over the wall.")})
      (= r 3) {:to to
               :payload (str "The gun jammed, " (:from arg) " got lucky.")})))

(def properties {:name "roulette"
                 :matches ["roulette" "rr"]
                 :dispatch roulette})