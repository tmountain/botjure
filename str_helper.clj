(ns str-helper)

(defmacro str-include? 
  "Returns true if string contains match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. contains (. ~match toLowerCase))))

(defmacro str-startswith? 
  "Returns true if string starts with match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. startsWith (. ~match toLowerCase))))

(defmacro str-endswith? 
  "Returns true if string starts with match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. endsWith (. ~match toLowerCase))))

(defn parse-username-txt [username]
     (if username
       (.substring username 1)))

(defn parse-privmsg
  [msg]
  (let [[_ to _ addr & text] (re-find #"(\S+)\s:((\S+):\s)*((\S+)\s)?(.+)?" msg)
        payload {:to to :addr addr}]
    (if (nil? (first text))
      (merge payload {:cmd (last text) :msg ""})
      (merge payload {:cmd (second text) :msg (last text)}))))

(defn parse-join
  [msg]
  (let [[_ chan] (re-find #":(\S+)" msg)]
    {:chan chan}))

(defn parse-action
  [matcher line]
  (let [[match user n action] (re-groups matcher)
        parsed {:from user :n n :action action :msgtype :event}
        msg (.substring line (count match))]
    (cond (= action "PRIVMSG")
          (merge parsed (parse-privmsg msg))
          (= action "JOIN")
          (merge parsed (parse-join msg)))))

(defn parse-info-str
  [msg]
  {:msg msg})

(defn parse-info
  [matcher line]
  (let [[match server code me] (re-groups matcher)]
    (merge {:server server :code code :me me}
           (parse-info-str (.substring line (count match))))))

(defn parse-server
  [matcher line]
  (let [[_ cmd txt] (re-groups matcher)
        payload {:cmd cmd :msg txt :msgtype :server}]
    {:cmd cmd :msg txt :msgtype :server}))

(defn parse-line
  [line]
  (let [parsed {:line line}
        action-match (re-matcher #"^:(\S+)!n=(\S+)\s(\S+)\s" line)
        info-match (re-matcher #"^:(\S+)\s(\d{3})\s(\S+)\s" line)
        server-match (re-matcher #"^([^: ]+)\s(.+)" line)]
    (cond (re-find action-match)
          (merge parsed (parse-action action-match line))
          (re-find info-match)
          (merge parsed (parse-info info-match line))
          (re-find server-match)
          (merge parsed (parse-server server-match line))
          :else (println "whoa"))))