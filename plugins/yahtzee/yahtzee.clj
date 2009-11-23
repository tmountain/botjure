;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yahtzee ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;  Written by Travis Whitton <tinymountain at gmail dot com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(ns yahtzee
  (:use [clojure.contrib.str-utils]))

;; utility functions ;;

(defn roll-die []
  "returns an integer in the range 1-6"
  (inc (rand-int 6)))

(defn roll-n-dice [number]
  "returns a lazy seq of requested number of dice rolls"
  (take number (repeatedly roll-die)))

(defn replace-dice [dice repl]
  "replaces values inside dice by those provided in repl"
  (for [i (range (count dice))]
    (if (repl i) (repl i) (nth dice i))))

(defn expand-roll [roll]
  "expands ranges embedded in a roll
   1-4,5 -> 1,2,3,4,5"
  (let [roll-seq (re-seq #"(\d)-(\d)" roll)]
    (if (empty? roll-seq)
      roll
      (let [start-idx (nth (first roll-seq) 1)
            end-idx (nth (first roll-seq) 2)
            exp-seq (range (Integer/parseInt start-idx)
                           (inc (Integer/parseInt end-idx)))]
        (if (empty? exp-seq)
          nil
          (str-join "," exp-seq))))))

(defn flatten 
  "Takes any nested combination of sequential things (lists, vectors, 
  etc.) and returns their contents as a single, flat sequence. 
  (flatten nil) returns nil." 
  [x] 
  (filter (complement sequential?) 
          (rest (tree-seq sequential? seq x)))) 

(defn parse-roll [roll]
  "parses a roll input by the user converting index to zero based
   (supports expansion via expand-roll)
   r 1,2,3 -> {1 true, 2 true, 3 true}"
   (if (< (count roll) 1)
     {}
     (let [tokens
             (flatten
               (filter identity
                 (map #(re-split #"," %)
                   (filter identity
                     (map expand-roll 
                       (re-split #"," roll))))))]
       (loop [i 0 mappings {}]
         (if (>= i (count tokens))
           mappings
           (recur (inc i) (assoc mappings
                            (dec (Integer/parseInt (nth tokens i)))
                             true)))))))

(defn group-by-occur [dice]
  "returns a map of occurrences by number for a given throw
   [1 1 2 4 4] -> {4 2, 2 1, 1 2}"
  (reduce
    (fn [acc it]
      (let [occur (acc it)]
        (assoc acc it
          (if occur
            (inc occur) 1)))) {} dice))

(defn ordered-keys [occur]
  "returns the keys from given sequence in descending order"
  (reverse (sort (keys occur))))

;; scoring functions ;;

(defn upper-section-score [dice box]
  "total number of dice faces matching box given (sum of dice)"
  (apply + (filter #(= box %) dice)))

(defn n-of-a-kind [dice number]
  "[number] dice showing the same face (sum of dice)"
  (let [occur (group-by-occur dice)
        seen (ordered-keys occur)]
    (loop [elem seen]
      (if (empty? elem)
        0
      (if (>= (occur (first elem)) number)
        (apply + dice)
        (recur (rest elem)))))))

(defn three-of-a-kind [dice]
  "three of a kind (sum of dice)"
  (n-of-a-kind dice 3))

(defn four-of-a-kind [dice]
  "four of a kind (sum of dice)"
  (n-of-a-kind dice 4))

(defn full-house [dice]
  "three of a kind and a pair (25 points)"
  (let [occur (group-by-occur dice)]
    (if (and (= 2 (count occur))
             (< 1 (val (first occur)) 4)) ; val is in range 2-3
      25
      0)))

(defn contains-series? [dice series]
  "helper for small-straight and large-straight"
  (if (some identity
    (map #(every? boolean
      (map (set dice) %1)) series))
    true
    false))

(defn small-straight [dice]
  "four sequential dice (30 points)"
  (if (contains-series? dice [[1 2 3 4]
                              [2 3 4 5]
                              [3 4 5 6]])
    30
    0))

(defn large-straight [dice]
  "five sequential dice (40 points)"
  (if (contains-series? dice [[1 2 3 4 5]
                              [2 3 4 5 6]])
    40
    0))

(defn yahtzee [dice]
  "five dice showing the same face (50 points)"
  (if (apply = dice)
    50
    0))

(defn chance [dice]
  "any combination (sum of dice)"
  (apply + dice))

;; output formatting

(defn shorten-if [row len]
  "shorten the given row if longer than length"
  (subs row
    0 (if (> (count row) len)
        len
        (count row))))

(defn lengthen-if [row len]
  "lengthen the given row if shorter than length"
  (if (< (count row) len)
    (str row (str-join "" (for [x (range (- len (count row)))] " ")))
    row))

(defn output-format [fmt table]
  "format the given table against the format string given"
  (let [lengths (map count (re-split #" " fmt))]
    (map 
     #(for [i (range (count %1))]
        (lengthen-if
          (shorten-if (nth %1 i) (nth lengths i))
          (nth lengths i))) table)))
      
(defn print-output [table]
  "print a formatted table"
  (doseq [row table]
    (println (str-join " " row))))

;; game stuffs

(def score-template {:aces    {:name "Aces",        :score nil},
                     :twos    {:name "Twos",        :score nil},
                     :threes  {:name "Threes",      :score nil},
                     :fours   {:name "Fours",       :score nil},
                     :fives   {:name "Fives",       :score nil},
                     :sixes   {:name "Sixes",       :score nil},
                     :toak    {:name "3 of a kind", :score nil},
                     :foak    {:name "4 of a kind", :score nil},
                     :full    {:name "Full House",  :score nil},
                     :sstrt   {:name "Sm Straight", :score nil},
                     :lstrt   {:name "Lg Straight", :score nil},
                     :yaht    {:name "YAHTZEE",     :score nil},
                     :chance  {:name "Chance",      :score nil}})

(def state-template {:turn 1,
                     :roll 0,
                     :dice [0 0 0 0 0]})

(defn print-scorecard [scard]
  (let [key-order [:aces :twos :threes :fours :fives :sixes
                   :toak :foak :full :sstrt :lstrt :yaht :chance]
        val-or-zero (fn [x] (if x x "empty"))
        fmt "-------------- -----"]
    (println "\nScore Boxes")
    (println "--------------------")
    (print-output
      (output-format fmt
        (for [box key-order]
          [(-> scard box :name), (val-or-zero (-> scard box :score))])))
    (print "\n")))

(defn print-state [gstate]
  (cond
    (= :turns-over gstate) (println
                             "\nOnly three rolls are allowed per turn.\n")
    (= :roll-wtf gstate) (println
                             "\nInvalid roll syntax.\n")
    (map? gstate) (println "\n(Turn" (str (gstate :turn) ",")
                               "Roll" (str (gstate :roll) ",")
                               "Dice" (str (gstate :dice) ")") "\n")))
(defn print-roll-help []
  (println (str "\nRoll commands may be issued in the following formats:\n"
                "r * (roll all dice)\n"
                "r n (roll nth die)\n"
                "r n,n (roll dice specified in list)\n"
                "r n-n (roll dice specified in range)\n"
                "The list and range forms may be combined if needed.\n")))

(defn print-score-help []
  (let [key-order [:aces :twos :threes :fours :fives :sixes
                   :toak :foak :full :sstrt :lstrt :yaht :chance]
        fmt "--------- ----------------------------"]
    (println "\nScore commands may be issued in the following formats:")
    (print-output
      (output-format fmt
        (for [box key-order]
          [(str "s " box),
           (str "(score round in " (-> score-template box :name) " box)")])))
    (print "\n")))

(defn score [cmd gstate scard]
  (let [cmd (keyword cmd)
        uppers #{:aces :twos :threes :fours :fives :sixes} ; upper boxes
        n-kind #{:toak :foak} ; lower boxes w/ two args
        lowers #{:full :sstrt :lstrt :yaht :chance} ; lower boxes w/ one arg
        box     {:aces 1, :twos 2, :threes 3, :fours 4, :fives 5, :sixes 6}
        fns     {:full full-house, :sstrt small-straight, :lstrt large-straight,
                 :yaht yahtzee, :chance chance}
        res (cond
              (contains? uppers cmd)
                (str (upper-section-score (@gstate :dice) (box cmd)))
              (contains? n-kind cmd)
                (str (n-of-a-kind (@gstate :dice) (if (= cmd :toak) 3 4)))
              (contains? lowers cmd)
                (str ((fns cmd) (@gstate :dice))))]
    (if (@scard cmd)
      (if-not (-> @scard cmd :score)
        (dosync
          (alter gstate assoc :turn (inc (@gstate :turn))
                              :roll 0
                              :dice [0 0 0 0 0])
          (alter scard assoc-in [cmd :score] res))))))

(defn roll [cmd gstate]
  (if (< (@gstate :roll) 3)
    (let [cmd (if (or (= (@gstate :roll) 0)
                      (= cmd "*")) "1-5" cmd)
          dice (vec (roll-n-dice 5))
          parsed-roll (parse-roll cmd)
          merged-roll (vec (for [i (range 5)]
                             (if (parsed-roll i)
                              (nth dice i)
                              (nth (@gstate :dice) i))))]
      (if (empty? parsed-roll)
        :roll-wtf
        (dosync
          (alter gstate assoc :dice merged-roll
                              :roll (inc (@gstate :roll))))))
    :turns-over))

(defn sanitize-cmd [cmd]
  (re-gsub #"[^*1-5,-]" "" cmd))

(defn sanitize-cmd-arg [cmd]
  (re-gsub #"[^a-z]" "" cmd))

(defn status [scard gstate]
  (do (print-scorecard @scard)
      (print-state @gstate)))

(defn eval-cmd [cmd scard gstate]
  (let [cmd-arg (second (re-split #" " cmd))]
    (cond
      (= \r (first cmd)) (if (empty? (sanitize-cmd cmd))
                           (print-roll-help)
                           (print-state (roll (sanitize-cmd cmd) gstate)))
      (= \s (first cmd)) (if (empty? cmd-arg)
                           (print-score-help)
                           (do (score (sanitize-cmd-arg cmd-arg)
                                       gstate scard) (status scard gstate)))
      (= \t (first cmd)) (status scard gstate)
      (= \h (first cmd)) nil)))

(defn game-loop []
  (let [scard (ref score-template)
        gstate (ref state-template)]
  (println "Welcome to Yahtzee!\n")
  (loop []
    (when (< (@gstate :turn) 14)
      (print "[(r)oll (s)core s(t)atus (h)elp]> ")
      (flush)
      (let [cmd (str (read-line))]
        (eval-cmd cmd scard gstate))
      (recur)))
  (println "Thanks for playing!\n")
  (status scard gstate)))

(game-loop)
