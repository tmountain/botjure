(ns plugins.echo)
(declare echo)

(defn echo [& args]
  (apply str args))

(def properties { :name      "echo",
                  :matches   :all,
                  :dispatch  echo })
