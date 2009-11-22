(ns botjure
  (:use irc-protocol)
  (:use plugin-loader)
  (:use str-helper))

(def config {:bot-name "ginkle"
             :channel  "#mindhed"
             :server   "irc.freenode.net"
             :port     6667})

(def connection (awaken config))

(defn event-loop []
  (loop [conn connection
         line (sock-read-line conn)]

    (println line)

    ; FIXME: move to a plugin
    (if (str-startswith? line "ping :")
      (let [resp (str "PONG " (.substring line (+ (.indexOf line ":") 1) ))]
          (sock-send conn resp)
          (println resp)))
    
    (if (str-startswith? line ":")
      (doseq [result (dispatch plugins line)]
        (if (:payload result)
          (privmsg conn (:to result) (:payload result)))))

    (recur conn (sock-read-line conn))))

(event-loop)
