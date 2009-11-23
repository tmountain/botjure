(ns plugins.echo)
(declare echo)

(defn echo [arg]
  (if (and (:to arg) (:from arg) (:n arg) (:msg arg) (:with arg)) 
    {:payload (:msg arg), :to (:to arg)}))

(def properties { :name      "echo",
                  :matches   "echo",
                  :dispatch  echo })
