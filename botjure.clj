(ns botjure
  (:use irc-utils)
  (:use plugin-helper)
  (:use plugin-loader)
  (:use str-helper))

(def config {:bot-name "ginkle"
             :channel  "#mindhed"
             :server   "irc.freenode.net"
             :port     6667})

(def connection (awaken config))

(defn plugin-error [e conn config]
  (. e printStackTrace)
  (privmsg conn (:channel config) 
           (str "Time for a plugin funeral.")))

(defn event-loop [config]
  (loop [conn connection
         line (sock-read-line conn)]

    (println line)

    (if (str-startswith? line "ping :")
      (let [resp (str "PONG " (.substring line (+ (.indexOf line ":") 1) ))]
          (sock-send conn resp)
          (println resp)))
    
    (if (str-startswith? line ":")
      (doseq [result (dispatch config plugins line)]
        (try
          (await result)
          (if (:payload @result)
            (if (contains? @result :method)
              ((:method @result) @result conn)
              (privmsg conn @result)))
          (clear-agent-errors result)
          (catch RuntimeException err (plugin-error err conn config))
          (catch Exception err (plugin-error err conn config)))))

    (recur conn (sock-read-line conn))))

(event-loop config)
