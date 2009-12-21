(ns plugin-helper
  (:use irc-utils))

(defmulti do-privmsg 
  "Sends a private message to the specified target"
  (fn [socket to msg] 
    ; fix for compatibility with clojure 1.0
    (if (or (vector? msg) (seq? msg))
      :collection
      :string)))

(defmethod do-privmsg :string
  [socket to msg]
  (sock-send socket (str "PRIVMSG " to " :" msg)))

(defmethod do-privmsg :collection 
  [socket to msg]
  (doseq [x msg]
    (sock-send socket (str "PRIVMSG " to " :" x) )
    (Thread/sleep 5000)))

(defn privmsg
  [socket result]
  (do-privmsg socket (:to result) (:payload result)))



