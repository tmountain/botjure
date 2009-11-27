(ns str-helper)

(defmacro str-include? 
  "Returns true if string contains match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. contains (. ~match toLowerCase))))

(defmacro str-startswith? 
  "Returns true if string starts with match or false otherwise"
  [string match]
  `(-> (. ~string toLowerCase) (. startsWith (. ~match toLowerCase))))

(defn squeeze [string character]
  (apply str
         (reverse
          (reduce
           (fn [x y]
             (if (and (= (first x)
                         character)
                      (= y character))
               x
               (conj x y)))
           (cons '() (seq string))))))

(defn parse-username-txt [username]
     (if username
       (.substring username 1)))

(defn parse-msg-txt [msg]
  (if msg
    (let [msg-txt (.substring msg 1)
          match (re-matcher #"(\S+):\s*(\S+)\s*(.*)" msg-txt)]
      (if (re-find match)
        (let [[_ addr cmd txt] (re-groups match)]
          {:msg txt
           :cmd cmd
           :addr addr})
        
        (let [match (re-matcher #"\s*@(\S+)\s*(.*)" msg-txt)]
          (if (re-find match)
            (let [[_ cmd txt] (re-groups match)]
              {:msg txt
               :cmd cmd})))))))

(defn parse-msg [line]
  "Returns a map of extracted values from a message"
  (let [[user type channel msg] (seq (.split line " " 4))]
    (let [[username n] (seq (.split user "!"))]
      (merge
       {:from (parse-username-txt username)
        :n n
        :to channel
        :with type}
       (parse-msg-txt msg)))))
