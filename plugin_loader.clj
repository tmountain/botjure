(ns plugin-loader
  (:use str-helper))

(def plugins '(plugins.counter plugins.echo plugins.dance plugins.hello plugins.roll plugins.roulette plugins.eightball))
(apply require plugins)


(defn apply-match [msg matches bot-name]
  (or
   (= matches :all)
   (and
    (if (contains? msg :addr)
      (= (:addr msg) bot-name)
      :true)
    (not
     (empty? (filter #(= (:cmd msg) %1) matches))))))

(defn dispatch [config plugins msg]
  (loop [plugins plugins
         msg (merge (parse-msg msg) config)
         payload []]
    (let [plugin (first plugins)]
      (if (not plugin)
        payload
        (let [properties (var-get (ns-resolve plugin 'properties))
              msg-from (if (contains? msg :from) (:from msg) nil)]
          ; in order for a plugin to dispatch, the conditions must be met:
          ; msg-txt & msg-from are non-nil values
          ; the message isn't from the bot
          ; the plugin's :matches value is within the messages text
          (if (and (:cmd msg) msg-from (not (= msg-from (:bot-name config)))
            (apply-match msg (:matches properties) (:bot-name config)))
            (recur (rest plugins) msg 
                   (conj payload (merge {:plugin (:name properties)}
                                        ((:dispatch properties) msg))))
            (recur (rest plugins) msg payload)))))))
