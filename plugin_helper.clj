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
  (println (str "PRIVMSG " to " :" msg))
  (sock-send socket (str "PRIVMSG " to " :" msg)))

(defmethod do-privmsg :collection 
  [socket to msg]
  (doseq [x msg]
    (println (str "PRIVMSG " to " :" x))
    (sock-send socket (str "PRIVMSG " to " :" x) )
    (Thread/sleep 5000)))

(defn privmsg
  [socket result]
  (do-privmsg socket (:to result) (:payload result)))

(defn srvmsg
  [socket result]
  (println (:payload result))
  (sock-send socket (:payload result)))

