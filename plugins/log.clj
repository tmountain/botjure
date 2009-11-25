(ns plugins.log)
(declare log)

(def the-log (ref []))

(defn update-the-log [alog line fifo-size]
  (dosync
   (if (> (count @alog) fifo-size)
     (alter alog #(vec (rest %1))))
   (alter alog conj line)))

(defn reveal-the-log [to cnt]
  {:payload (take cnt @the-log) :to to})

(defn log-dispatch [arg]
  (if (= (:to arg) (:channel arg))
    (do (update-the-log the-log (:msg arg) 5)
        {})))

(def properties {:name "log"
                 :matches :all
                 :dispatch log-dispatch})
