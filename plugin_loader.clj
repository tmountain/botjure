(ns plugin-loader
  (:use str-helper))

(def plugins '(plugins.counter plugins.echo))
(apply require plugins)

(defn dispatch [plugins msg]
  (loop [plugins plugins
         msg msg
         payload []]
    (let [plugin (first plugins)]
      (if (not plugin)
        payload
        (let [properties (var-get (ns-resolve plugin 'properties))]
          (if (or (= (:matches properties) :all)
                  (not (empty? (filter #(= msg %1) (:matches properties)))))
            (recur (rest plugins) msg 
                   (conj payload (merge {:plugin (:name properties)}
                                        ((:dispatch properties) (parse-msg msg)))))
            (recur (rest plugins) msg payload)))))))
