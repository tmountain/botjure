(ns plugins.bowling
  (:use clojure.contrib.seq-utils))
(use '[clojure.contrib.str-utils :only (str-join)])
(use '[clojure.contrib.math :only (floor)])


;; Bowling scoring functions from
;; http://github.com/stuarthalloway/clojure-bowling/blob/master/src/bowling_game.clj

(defn strike? [rolls]
  (= 10 (first rolls)))

(defn spare? [rolls]
  (= 10 (apply + (take 2 rolls))))

(defn balls-to-score
  "How many balls contribute to this frame's score?"
  [rolls]
  (cond
    (strike? rolls) 3
    (spare? rolls) 3
    :else 2))

(defn frame-advance
  "How many rolls should be consumed to advance to the next frame?"
  [rolls]
  (if (strike? rolls)
    1 2))

(defn frames
  "Converts a sequence of rolls to a sequence of frames"
  [rolls]
  (when-let [rolls (seq rolls)]
    (lazy-seq (cons (take (balls-to-score rolls) rolls)
                    (frames (drop (frame-advance rolls) rolls))))))

(defn score-frame
  [frame]
    (reduce + frame))

(defn score-game
  "Score a bowling game, passed as a sequence of rolls."
  [rolls]
    (reduce + (map score-frame (take 10 (frames rolls)))))

;; Beginning of custom functions.

(def score-card (ref {}))

(defn init-player-score
  "Initialize the player's scorecard."
  [player]
  (dosync (alter score-card assoc player [])))

(defn init-score-card
  "Initialize the game's scorecard."
  []
  (dosync (ref-set score-card {})))

(defn players-frames
  [players-scores]
  (into {}
        (for [player (keys players-scores)]
          [player (frames (get players-scores player))])))

(defn game-on?
  "Answer the question: Is the game currently active or not?"
  [frames]
  (if (> (reduce + (map #(count %) (vals frames))) 0)
    :true))

(defn join-game
  "Join a player to the game."
  [player]
  (if (not (game-on? (players-frames @score-card)))
    (init-player-score player)))

(defn start-game
  "Start the game."
  []
  (init-score-card))

(defn current-round
  "Answer the question: What round is the game in?"
  [frames]
  (apply max (map #(count %) (vals frames))))

(defn do-current-round
  "Express what round the game is currently in."
  []
  (str "We are currently playing round " (int (current-round (frames @score-card))) "."))


(defn update-score-card
  "Add a new score to the player's score card."
  [player score]
  (dosync
   (alter score-card assoc player (conj (get @score-card player) score))))

(defn in-frame?
  "Answer the question: Are we in the middle of a frame?"
  [frame]
  (and
   (= (count frame) 1)
   (not (= (first frame) 10))))

(defn players-in-frame
  "Answer the question: Which players are mid-fame?"
  [frames]
  (filter #(in-frame? (last (get frames %))) (keys frames)))

(defn player-in-frame?
  "Answer the question: Is the give player mid-frame?"
  [player]
  (contains? (set (players-in-frame (players-frames @score-card))) player))

(defn new-frame?
  "Answer the question: Is the given player starting a new frame?"
  [player]
  (not (player-in-frame? player)))

(defn players-with-turn
  "Answer the question: Who can currently take a turn?"
  [frames]
  (let [prev-round (dec (current-round frames))]
    (if (= prev-round -1)
      (keys frames)
      (filter #(= prev-round (count (get frames %))) (keys frames)))))

(defn players-at-endgame
  "Answer the question: Which players are at the end-game?"
  [frames]
  (filter #(>= (count (get frames %)) 10) (keys frames)))

(defn eligible-players
  "Aswer the question: Which players can roll the ball right now, for whatever reason?"
  [player-scores]
  (let [frames (players-frames player-scores)]
    (set (concat (players-in-frame frames) (players-with-turn frames) (players-at-endgame frames)))))

(defn player-game-over?
  "Asnwer the question: For the given user, is the game over?"
  [player]
  (let [frames (frames (get @score-card player))
        count (count frames)
        last-frame (last frames)
        next-last-total (apply + (last (butlast frames)))]
    (or (= count 12)
        (and (= count 11) (< (first last-frame) 10))
        (and (= count 10) (< next-last-total 10)))))

(defn game-over?
  "Answer the question: Is the game over?"
  []
  (let [players-games-over (filter #(not (player-game-over? %)) (keys @score-card))]
    (empty? players-games-over)))

(defn current-frame-score
  "Answer the question: What is the given player's current mid-frame score?"
  [player]
  (if (player-in-frame? player)
    (last (get @score-card player))
    0))

(defn pins-remaining
  "Answer the question: For the given user, how many pins are remaining?"
  [player]
  (- 10 (current-frame-score player)))

(defn roll-the-ball
  "Roll the ball for the given player"
  [player]
  (rand-int
   (if (new-frame? player)
     11
     (inc (pins-remaining player)))))

(defn do-join-game
  "Join the given user to the existing game, if possible."
  [player]
  (if (join-game player)
    (str "You have joined the current game, " player)
    (str "Sorry, " player ".  The game has already begun.")))

(defn do-start-game
  "Start a new game and join the given user to it."
  [player]
  (start-game)
  [(str player " has started a new round of bowling.")
   (do-join-game player)])

(defn knock-pins-down
  "Kock pins down for a given player and remaining pins."
  [player num]
  (update-score-card player num)
  (cond
    (= num 0)(str player ": Gutter ball...")
      (= num 1) (str player ": One Pin")
      (= num 2) (str player ": Two Pin")
      (= num 3) (str player ": Three Pin")
      (= num 4) (str player ": Four Pin")
      (= num 5) (str player ": Five Pin")
      (= num 6) (str player ": Six Pin")
      (= num 7) (str player ": Seven Pin")
      (= num 8) (str player ": Eight Pin")
      (= num 9) (str player ": Nine Pin")
      (= num 10) (str player ": Strike!")))

(defn do-player-status
  "Report the status of the game for a given user."
  [player]
  (let [game-over (player-game-over? player)
        pins (pins-remaining player)
        in-frame (not (new-frame? player))
        frame-count (count (get (players-frames @score-card) player))
        frame-num (if in-frame frame-count (inc frame-count))]
    (if game-over
      (str player " has bowled their last frame for the game.")
      (str player " is on frame " frame-num " with " pins " pins remaining"))))

(defn do-statuses
  "Report the status of the game for all users."
  [score-card]
  (map #(do-player-status %) (keys score-card)))

(defn do-score
  "Report the current score for a given user."
  [player]
  (str player " current has " (score-game (get @score-card player)) " points."))

(defn do-player-score
  "Report the current score for all users."
  [score-card]
  (map #(do-score %) (keys score-card)))

(defn do-game-over
  "Report that the game is over and show the scores."
  []
  (concat [(str "The game has come to a close.")] (do-score @score-card)))

(defn do-bowl
  "Handle simple queries and pass handled events to the game dispatch."
  [player player-scores]
  (let [player-cnt (count (keys player-scores))
        eligible (eligible-players player-scores)]
    (cond
      (nil? (player-scores player))
      (if (game-on? (players-frames player-scores))
        (str "Sorry, " player ".  The game has started and you are not in it.")
        (str "You have not yet joined the game, " player "."))
      (< player-cnt 2)
      (str "Sorry, " player ".  There are not enough players in the game.")
      (and (not (empty? eligible))
           (not (contains? eligible player)))
      [(str "Sorry, " player ".  It is not yet your turn.")
       (str "Players who can take a turn: " (str-join ", " eligible))]
      (game-over?)
      (do-game-over)
      (player-game-over? player)
      (str "Sorry, " player ".  Your game has finished.")
      :else
      (let [num-pins (roll-the-ball player)]
        (knock-pins-down player num-pins)))))


(defn bowl-dispatch
  "Dispatch the event for each handled game action."
  [{player :from msg :msg to :to}]
  (assoc {:to to} :payload
         (cond
          (= msg "start")
          (do-start-game player)
          (= msg "join")
          (do-join-game player)
          (= msg "score")
          (do-player-score player)
          (= msg "scores")
          (do-score @score-card)
          (= msg "status")
          (do-player-status player)
          (= msg "statuses")
          (do-statuses @score-card)
          :else
          (do-bowl player @score-card))))


(def properties {:name "bowling"
                 :matches ["bowl"]
                 :dispatch bowl-dispatch})
