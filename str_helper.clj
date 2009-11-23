(ns str-helper)

;; (defn parse-msg 
;;   [line]
;;   "Returns a map of extracted values from a message"
;;   (let [[user type channel msg] (seq (.split line " " 4))]
;;     (let [[username n] (seq (.split user "!"))]
;;       (let [from (if username (.substring username 1) nil)
;;             msg-txt (if msg (.substring msg 1) nil)]
;;         (if msg-txt
;;           (let [match (re-matcher #"(\S+):\s*(.*)" msg-txt)]
;;             (if (re-find match)
;;               (let [[_ username msg-trim] (re-groups match)]
;;                 {:from from,
;;                  :n n,
;;                  :to channel,
;;                  :with type,
;;                  :addr username,
;;                  :msg msg-trim})
;;               {:from from,
;;                :n n,
;;                :to channel,
;;                :with type,
;;                :msg msg})))))))

(defn parse-username-txt [username]
     (if username
       (.substring username 1)))

(defn parse-msg-txt [msg]
  (if msg
    (let [msg-txt (.substring msg 1)
          match (re-matcher #"(\S+):\s*(.*)" msg-txt)]
      (if (re-find match)
        (let [[_ addr txt] (re-groups match)]
          {:txt txt
           :addr addr})
        {:txt msg-txt}))))

(defn parse-msg [line]
  "Returns a map of extracted values from a message"
  (let [[user type channel msg] (seq (.split line " " 4))]
    (let [[username n] (seq (.split user "!"))]
      {:from (parse-username-txt username)
       :n n
       :to channel
       :with type
       :msg (parse-msg-txt msg)})))


(defmacro str-include? 
  "Returns true if string contains match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. contains (. ~match toLowerCase))))

(defmacro str-startswith? 
  "Returns true if string starts with match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. startsWith (. ~match toLowerCase))))
