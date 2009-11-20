(ns plugins.echo)
(declare echo)

(defn echo [arg]
  {:payload (:msg arg),
   :to (:to arg)})

(def properties { :name      "echo",
                  :matches   :all,
                  :dispatch  echo })
