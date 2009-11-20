(ns plugins.echo)
(declare echo)

(defn echo [& args]
  {:payload (apply str args)})

(def properties { :name      "echo",
                  :matches   :all,
                  :dispatch  echo })
