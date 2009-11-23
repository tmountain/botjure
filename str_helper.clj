(ns str-helper)

(defn parse-msg 
  [line]
  "Returns a map of extracted values from a message"
  (let [[user type channel msg] (seq (.split line " " 4))]
    (let [[username n] (seq (.split user "!"))]
      (let [from (if username (.substring username 1) nil)
            msg-txt (if msg (.substring msg 1) nil)]
        (if msg-txt
          (let [match (re-matcher #"(\S+):\s*(.*)" msg-txt)]
            (if (re-find match)
              (let [[_ username msg-trim] (re-groups match)]
                {:from from,
                 :n n,
                 :to channel,
                 :with type,
                 :addr username,
                 :msg msg-trim})
              {:from from,
               :n n,
               :to channel,
               :with type,
               :msg msg-txt})))))))

(defmacro str-include? 
  "Returns true if string contains match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. contains (. ~match toLowerCase))))

(defmacro str-startswith? 
  "Returns true if string starts with match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. startsWith (. ~match toLowerCase))))
