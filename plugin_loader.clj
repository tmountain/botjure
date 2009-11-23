(ns plugin-loader
  (:use str-helper))

(def plugins '(plugins.counter plugins.echo))
(apply require plugins)

(defn dispatch [config plugins msg]
  (loop [plugins plugins
         msg (parse-msg msg)
         payload []]
    (let [plugin (first plugins)]
      (if (not plugin)
        payload
        (let [properties (var-get (ns-resolve plugin 'properties))
              msg-txt (if (contains? msg :msg) (:msg msg) nil)
              msg-from (if (contains? msg :from) (:from msg) nil)]
          ; in order for a plugin to dispatch, the conditions must be met:
          ; msg-txt & msg-from are non-nil values
          ; the message isn't from the bot
          ; the plugin's :matches value is within the messages text
          (if (and msg-txt msg-from (not (= msg-from (:bot-name config)))
            (or (= (:matches properties) :all)
                  (not (empty? (filter #(str-include? msg-txt %1)
                                        (:matches properties))))))
            (recur (rest plugins) msg 
                   (conj payload (merge {:plugin (:name properties)}
                                        ((:dispatch properties) msg))))
            (recur (rest plugins) msg payload)))))))
