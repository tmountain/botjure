(ns irc-protocol
  (:import
     (java.net Socket)
     (java.io BufferedReader)
     (java.io BufferedWriter)
     (java.io InputStreamReader)
     (clojure.lang PersistentVector)))

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

(defn sock-recv-seq 
  "Returns a lazy sequence of lines from the provided socket"
  [socket]
  (let [reader (:reader socket)]
    (line-seq reader)))

(defmacro sock-recv 
  "Returns a string from the provided socket"
  [socket]
  `(reduce (fn [x# y#] (str x# y#)) "" (sock-recv-seq ~socket)))

(defn close 
  "Closes the provided socket"
  [socket]
  (. socket close))

(defn join 
  "Joins the specified channel"
  [socket channel]
  (sock-send socket (str "JOIN " channel)))

(defn notice 
  "Issues a notice to the specified target"
  [socket target msg]
  (sock-send socket (str "NOTICE " target " :" msg)))

(defmulti privmsg 
  "Sends a proviate message to the specified target"
  (fn [socket to msg] (class msg)))

(defmethod privmsg String
  [socket to msg]
  (sock-send socket (str "PRIVMSG " to " :" msg)))

(defmethod privmsg PersistentVector 
  [socket to msg]
  (doseq [x msg]
    (sock-send socket (str "PRIVMSG " to " :" x) )
    (Thread/sleep 5000)))

(defmacro str-include? 
  "Returns true if string contains match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. contains (. ~match toLowerCase))))

(defn whois 
  "Issues a whois command on the given nick"
  [socket nick]
  (sock-send socket (str "WHOIS " nick))
  (apply str (filter #(not (str-include? "end of whois" %1)) 
           (sock-recv-seq socket))))

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
