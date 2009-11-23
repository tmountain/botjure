(ns plugins.dance)
(declare dance)

(defn dance [arg]
  {:payload (vector ":D-<" ":D|-<" ":D/-<") :to (:to arg)})

(def properties { :name      "dance",
                  :matches   ["@dance"],
                 :dispatch  dance })
