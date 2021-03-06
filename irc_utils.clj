(ns irc-utils
  (:import
     (java.net Socket)
     (java.io BufferedReader)
     (java.io BufferedWriter)
     (java.io InputStreamReader)
     (clojure.lang PersistentVector))
  (:use str-helper))

(defn connect
  "Connect to specified server and port returning a socket"
  [server port]
  (let [socket (new Socket server port)]
    {:socket socket
     :reader (new BufferedReader
                    (new InputStreamReader
                         (.getInputStream socket)))
     :writer (.getOutputStream socket)}))

(defn sock-send
  "Sends msg to socket appending newline carriage-return"
  [socket msg]
  (let [writer (:writer socket)]
    (. writer write (. (str msg "\r\n") getBytes))
    (. writer flush)))

(defn sock-read-line [connection]
  (. (:reader connection) readLine))

(defn close 
  "Closes the provided socket"
  [socket]
  (. socket close))

(defn identify 
  "Identifies the bot associated with the current connection"
  [sock nick]
  (sock-send sock (str "USER " nick " localhost localhost :" nick))
  (sock-send sock (str "NICK " nick)))

(defn join 
  "Joins the specified channel"
  [socket channel]
  (sock-send socket (str "JOIN " channel)))

(defn awaken
  "Wakes up the bot"
  [config]
  (let [sock (connect (:server config) (:port config))]
      (identify sock (:bot-name config))
      (join sock (:channel config))
       sock))

(defn notice 
  "Issues a notice to the specified target"
  [socket target msg]
  (sock-send socket (str "NOTICE " target " :" msg)))

; FIXME: add a plugin to interpret IRC system data
; (defn whois 
;   "Issues a whois command on the given nick"
;   [socket nick]
;   (sock-send socket (str "WHOIS " nick))
;   (apply str (filter #(not (str-include? "end of whois" %1)) 
;            (sock-recv-seq socket))))

(defn disconnect
  "Disconnects from the provided socket"
  ([socket]
   (sock-send socket "QUIT :Disconnected"))
  ([socket msg]
   (sock-send socket (str "QUIT :" msg))))

(defn mode 
  "Sets the mode based on provided target and command"
  [socket target command]
  (sock-send socket (str "MODE " target " " command)))
