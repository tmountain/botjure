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
  [player]
  (dosync (alter score-card assoc player [])))

(defn init-score-card []
  (dosync (ref-set score-card {})))



;; (defn frames-count
;;   [count frames]
;;   (let [frame (first frames)]
;;     (if (nil? frame)
;;       count
;;       (let [weight (if (= frame 10) 1 0.5)]
;;         (frames-count (+ count weight) (rest frames))))))

;; (defn players-frames-count [players-scores]
;;   (into {}
;;         (for [player (keys players-scores)]
;;           [player (count (frames (get players-scores player)))])))

(defn players-frames
  [players-scores]
  (into {}
        (for [player (keys players-scores)]
          [player (frames (get players-scores player))])))

;(def player-history (ref {}))

;; (defn get-history
;;   [player]
;;   (if (contains? @player-history player)
;;     (get @player-history player)
;;     []))


;; start a thread with start-game that will auto-close the game after
;; a period of time.  Each bowl action resets the timer.

;; (defn players-turns-cnts []
;;   (into {}
;;         (for [player (keys @player-history)]
;;           [player (count (get-history player))])))

;; (defn players-lastframe-cnts []
;;   (into {}
;;         (for [player (keys @player-history)]
;;           [player (count (nth (get-history player) 0 []))])))

;; (defn sum-of-vals [x]
;;   (reduce + (map #(val %) x)))

(defn game-on? [frames]
  (if (> (reduce + (map #(count %) (vals frames))) 0)
    :true))

(defn join-game
  [player]
  (if (not (game-on? (players-frames @score-card)))
    (init-player-score player)))

(defn start-game []
  (init-score-card))

(defn current-round [frames]
  (apply max (map #(count %) (vals frames))))

(defn do-current-round []
  (str "We are currently playing round " (int (current-round (frames @score-card))) "."))

;; (defn players-mid-round [frames]
;;   (let [round (current-round frames)]
;;     (filter #(< (get frames %) round) (keys frames))))


;(defn close-game)

;; (defn update-score-card
;;   [player turn therest]
;;   (dosync
;;    (alter score-card assoc player
;;           (conj therest turn))))

;; (defn new-frame?
;;   [player]
;;   (let [history (get-history player)
;;         frame (first history)]
;;     (or (= (count frame) 2) (= (first frame) 10))))

(defn update-score-card
  [player score]
  (dosync
   (alter score-card assoc player (conj (get @score-card player) score))))


;; (defn in-frame?
;;   [count]
;;   (> (- count (floor count)) 0))

(defn in-frame?
  [frame]
  (and
   (= (count frame) 1)
   (not (= (first frame) 10))))

;; (defn new-frame?
;;   [player]
;;   (let [count (get (players-frames-count @score-card) player)]
;;     (not (in-frame? count))))

(defn players-in-frame
  [frames]
  (filter #(in-frame? (last (get frames %))) (keys frames)))

(defn player-in-frame?
  [player]
  (contains? (set (players-in-frame (players-frames @score-card))) player))

(defn new-frame?
  [player]
  (not (player-in-frame? player)))

(defn players-with-turn
  [frames]
  (let [prev-round (- (current-round frames) 1)]
    (if (= prev-round -1)
      (keys frames)
      (filter #(= prev-round (count (get frames %))) (keys frames)))))

(defn players-at-endgame
  [frames]
  (filter #(>= (count (get frames %)) 10) (keys frames)))

(defn eligable-players
  [player-scores]
  (let [frames (players-frames player-scores)]
    (set (concat (players-in-frame frames) (players-with-turn frames) (players-at-endgame frames)))))

;; (defn players-unfinished-frame []
;;   (for [[k v] (players-lastframe-cnts)
;;         :when (and
;;                (= v 1)
;;                (not (= (first (first (get @player-history k))) 10)))]
;;     k))

;; (defn players-queued []
;;   (let [players-cnts (players-turns-cnts)
;;         last-round (- (current-round players-cnts) 1)]
;;     (if (= last-round -1)
;;       (keys @player-history)
;;       (for [[k v] players-cnts :when (= last-round v)]
;;         k))))



;; (defn current-frame-score
;;   [player]
;;   (let [history (get-history player)]
;;     (if (and (not (nil? history)) (not (nil? (first history))))
;;       (first (first (get-history player)))
;;       0)))


(defn player-game-over?
  [player]
  (let [frames (frames (get @score-card player))
        count (count frames)
        last-frame (last frames)
        next-last-total (apply + (last (butlast frames)))]
    (or (= count 12)
        (and (= count 11) (< (first last-frame) 10))
        (and (= count 10) (< next-last-total 10)))))

(defn game-over?
  []
  (let [players-games-over (filter #(not (player-game-over? %)) (keys @score-card))]
    (empty? players-games-over)))

(defn current-frame-score
  [player]
  (if (player-in-frame? player)
    (last (get @score-card player))
    0))

(defn pins-remaining
  [player]
  (- 10 (current-frame-score player)))

(defn roll-the-ball
  [player]
  (rand-int
   (if (new-frame? player)
     11
     (+ (pins-remaining player) 1))))

(defn do-player-status
  [player]
  (let [game-over (player-game-over? player)
        pins (pins-remaining player)
        in-frame (not (new-frame? player))
        frame-count (count (get (players-frames @score-card) player))
        frame-num (if in-frame frame-count (+ frame-count 1))]
    (if game-over
      (str player " has bowled their last frame for the game.")
      (str player " is on frame " frame-num " with " pins " pins remaining"))))

(defn do-join-game [player]
  (if (join-game player)
    (str "You have joined the current game, " player)
    (str "Sorry, " player ".  The game has already begun.")))


(defn do-start-game [player]
  (start-game)
  [(str player " has started a new round of bowling.")
   (do-join-game player)])

(defn knock-pins-down
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

(defn do-score
  [player]
  (str player " current has " (score-game (get @score-card player)) " points."))

(defn do-scores []
  (map #(do-score %) (keys @score-card)))

(defn do-game-over []
  (concat [(str "The game has come to a close.")] (do-scores)))

(defn do-bowl
  [player player-scores]
  (let [player-cnt (count (keys player-scores))
        eligable (eligable-players player-scores)]
    (cond
      (nil? (player-scores player))
      (if (game-on? (players-frames player-scores))
        (str "Sorry, " player ".  The game has started and you are not in it.")
        (str "You have not yet joined the game, " player "."))
      (< player-cnt 2)
      (str "Sorry, " player ".  There are not enough players in the game.")
      (and (not (empty? eligable))
           (not (contains? eligable player)))
      [(str "Sorry, " player ".  It is not yet your turn.")
       (str "Players who can take a turn: " (str-join ", " eligable))]
      (game-over?)
      (do-game-over)
      (player-game-over? player)
      (str "Sorry, " player ".  Your game has finished.")
      :else
      (let [num-pins (roll-the-ball player)]
        (knock-pins-down player num-pins)))))


(defn bowl-dispatch
  [arg]
  (let [player (:from arg)
        msg (:msg arg)
        result {:to (:to arg)}]
    (assoc result :payload
           (cond
             (= msg "start")
             (do-start-game player)
             (= msg "join")
             (do-join-game player)
             (= msg "score")
             (do-score player)
             (= msg "scores")
             (do-scores)
             (= msg "status")
             (do-player-status player)
             :else
             (do-bowl player @score-card)))))


(def properties {:name "bowling"
                 :matches ["bowl"]
                 :dispatch bowl-dispatch})



