(ns plugin-loader)

(def plugins '(plugins counter echo))
(require plugins)

(defn dispatch [plugins msg]
  (loop [plugins plugins
         msg msg
         payload []]
    (let [plugin (first plugins)]
      (if (not plugin)
        payload
        (if (not (= 'plugins plugin))
          (let [properties 
                (eval (symbol (str "plugins." plugin "/properties")))]
            (if (or (= (:matches properties) :all)
                    (not (empty? (filter #(= msg %1) (:matches properties)))))
              (recur (rest plugins) msg 
                     (conj payload (merge {:plugin (:name properties)}
                                          ((:dispatch properties) msg))))
              (recur (rest plugins) msg payload)))
          (recur (rest plugins) msg payload))))))
