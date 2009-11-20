(ns plugin-loader)

(def plugins '(plugins counter))
(require plugins)

(defn dispatch [plugins pattern]
  (loop [plugins plugins
         pattern pattern
         payload []]
    (let [plugin (first plugins)]
      (if (not plugin)
        payload
        (if (not (= 'plugins plugin))
          (let [properties 
                (eval (symbol (str "plugins." plugin "/properties")))]
            (if (not (empty? (filter #(= pattern %1) (:matches properties))))
              (recur (rest plugins) pattern 
                     (conj payload ((:dispatch properties))))
              (recur (rest plugins) pattern payload)))
          (recur (rest plugins) pattern payload))))))
