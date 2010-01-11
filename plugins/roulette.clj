(ns plugins.roulette)
(use '[clojure.contrib.str-utils :only (str-join)])

(def score (ref {}))

(defn init-score [user]
  (dosync (alter score assoc user 1)))

(defn increment-score [user]
  (dosync (alter score assoc user (inc (get @score user)))))

(defn find-lucky []
  (apply min-key @score (keys @score)))

(defn find-unlucky []
  (apply max-key @score (keys @score)))

(defn reveal-score []
  [(str-join ", "(map #(str %1 ": " (get @score %1)) (keys @score)))
   (if (> (count (keys @score)) 1)
     (let [lucky (find-lucky) unlucky (find-unlucky)]
       (if (not (= lucky unlucky))
         (str lucky " is the Luckiest.  "
              unlucky " is the Unluckiest.")
         "Everyone is (Un)Lucky."))
     "There is only one person here shooting themselves.")])

(defn do-score [arg]
  {:to (:to arg) :payload (reveal-score)})

(defn do-roulette [arg]
  (let [r (rand-int 3)
        result {:to arg}]
    (cond
      (= r 0) (assoc result :payload (str "CLICK! " (:from arg) " got lucky."))
      (= r 1) (do
                (if (contains? @score (:from arg))
                  (increment-score (:from arg))
                  (init-score (:from arg)))
                (assoc result :payload (str "BANG! " (:bot-name arg) " watches " (:from arg) "'s brain splatter all over the wall.")))
      (= r 2) (assoc result :payload (str "The gun jammed, " (:from arg) " got lucky.")))))

(defn roulette-dispatch [arg]
  (if (= (:msg arg) "?")
    (do-score arg)
    (do-roulette arg)))

(def properties {:name "roulette"
                 :matches ["roulette" "rr"]
                 :dispatch roulette-dispatch})
