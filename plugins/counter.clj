(ns plugins.counter)
(declare counter)

(defn make-counter [init-val] 
  (let [c (atom init-val)] #(swap! c inc)))

(def counter (make-counter 0))

(def properties { :name      "count",
                  :matches   ["@count"],
                  :dispatch  counter })
