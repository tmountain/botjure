(ns botjure
  (:use irc-protocol)
  (:use plugin-loader)
  (:use str-helper))

(def config {:bot-name "ginkle"
             :channel  "#mindhed"
             :server   "irc.freenode.net"
             :port     6667})

(defn identify [sock nick]
  (sock-send sock (str "USER " nick " localhost localhost :" nick))
  (sock-send sock (str "NICK " nick)))

(defn sock-read-line [connection]
  (. (:reader connection) readLine))

(defn awaken
  "Wakes up the bot"
  [config]
  (let [sock (connect (:server config) (:port config))]
      (identify sock (:bot-name config))
      (join sock (:channel config))
       sock))

(def connection (awaken config))

(loop [conn connection
       line (sock-read-line conn)]

  (println line)

  (if (str-startswith? line "ping :")
    (let [resp (str "PONG " (.substring line (+ (.indexOf line ":") 1) ))]
        (sock-send conn resp)
        (println resp)))
  
  (if (str-startswith? line ":")
    (doseq [result (dispatch plugins line)]
      (if (:payload result)
        (privmsg conn (:to result) (:payload result)))))

  (recur conn (sock-read-line conn)))
