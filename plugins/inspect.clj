(ns plugins.inspect)
(use '[clojure.contrib.str-utils :only (str-join)])
(declare inspect)

(defn inspect [arg]
  (do
    (println (str-join ", "(map #(str %1 ": " (pr-str (%1 arg))) (keys arg))))))

(def properties {:name "inspect"
                  :matches :all
                  :dispatch inspect})

