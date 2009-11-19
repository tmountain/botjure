(ns irc-main
  (use irc-protocol))

(defn identify [sock nick]
  (sock-send sock (str "USER " nick " localhost localhost :" nick))
  (sock-send sock (str "NICK " nick)))


(defn awaken
  "Wakes up the bot"
  [server channel]
  (let [sock (connect server 6667)]
    (identify sock "binkle")
    (join sock channel)
    sock))

(def conn (awaken "irc.freenode.net" "#mindhed"))
