(ns plugins.counter)

(defn make-counter [init-val] 
  (let [c (atom init-val)] #(swap! c inc)))

(def stateful-counter (make-counter 0))

(defn counter [arg] {:payload (stateful-counter)
                     :to (:to arg)})

(def properties { :name      "count",
                  :matches   ["count"],
                  :dispatch  counter })
