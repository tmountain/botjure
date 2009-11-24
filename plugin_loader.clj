(ns plugin-loader
  (:use str-helper))

(def plugins '(plugins.counter plugins.echo plugins.dance plugins.hello plugins.roll))
(apply require plugins)


(defn apply-match [msg matches bot-name]
  (or
   (= matches :all)
   (and
    (= (:addr msg) bot-name)
    (not
     (empty? (filter #(str-include? (:msg msg) %1) matches))))
   (not
    (empty? (filter #(str-include? (:msg msg) %1) (map #(str "@" %1) matches))))))

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
            (apply-match msg (:matches properties) (:bot-name config)))
            (recur (rest plugins) msg 
                   (conj payload (merge {:plugin (:name properties)}
                                        ((:dispatch properties) msg))))
            (recur (rest plugins) msg payload)))))))
