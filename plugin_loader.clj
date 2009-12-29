(ns plugin-loader
  (:use str-helper))
(use '[clojure.contrib.str-utils :only (str-join)])
(def plugins '(plugins.hello plugins.roll plugins.roulette plugins.eightball
                             plugins.log plugins.inspect plugins.ping))
(apply require plugins)

(defn match-transform [match msg]
  "Transform the match and string into a useful match value (re-groups for regex, the match term for string match).  These values also serve as true/false for determining whether plugin dispatches will get fired."
  (if (= (type match) java.util.regex.Pattern)
    (re-find match (:line msg))
    (if (= match (:cmd msg))
      match
      nil)))

(defn match-list [msg matches bot-name]
  "Given a plugin's match list, the msg and the bot-name execute the match strategy to dispatch for a plugin."
  (if (= matches :all)
    (seq [:all])
    (if (or (= (:msgtype msg) :server)
            (and (= (:addr msg) bot-name)
                 (:from msg)
                 (not (= (:from msg) bot-name))))
      (let [m (filter #(if %1 :true)
                      (map #(match-transform %1 msg) matches))]
        (if (> (count m) 0)
          m
          nil)))))

(defn dispatch [config plugins msg]
  (loop [plugins plugins
         msg (merge (parse-line msg) config)
         payload []]

    (let [plugin (first plugins)]
      (if (not plugin)
        payload
        (let [properties (var-get (ns-resolve plugin 'properties))
;              msg-from (if (contains? msg :from) (:from msg) nil)
              matches (match-list msg (:matches properties) (:bot-name config))]
          ; in order for a plugin to dispatch, the conditions must be met:
          ; msg-txt & msg-from are non-nil values
          ; the message isn't from the bot
          ; the plugin's :matches value is within the messages text
          (if (and (:cmd msg)
                   matches)
            (recur (rest plugins) msg 
                   (conj payload (send-off (agent (merge msg {:matches matches}))
                                       #(merge {:plugin (:name properties)}
                                               ((:dispatch properties) %1)))))
            (recur (rest plugins) msg payload)))))))
