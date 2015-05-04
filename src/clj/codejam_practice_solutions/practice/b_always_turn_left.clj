;; Dashboard - Practice Problems - Google Code Jam
;; https://code.google.com/codejam/contest/32003/dashboard#s=p1
;; Problem B. Always Turn Left
(ns codejam-practice-solutions.practice.b-always-turn-left
  (:require [clojure.string :as s]))

(defonce forward "W") ; w for walk
(defonce right "R")
(defonce left "L")

(defn unknown-cell [] {:type :unknown})
(defn wall-cell [] {:type :wall})
(defn empty-cell [] {:type :empty})

(def south :south)
(def north :north)
(def east :east)
(def west :west)

(def input-small
  (s/split-lines
   (slurp "./src/clj/codejam_practice_solutions/practice/B-small-practice.in")))

(defn parse-input [input-line]
  (let [[entrance-to-exit exit-to-entrance]
        (s/split input-line #" ")]
    {:forward entrance-to-exit
     :backward exit-to-entrance}))

(defn player-at [x y facing]
  {:x x, :y y, :facing facing})

(defn replace-char [string position char]
  (str (subs string 0 position) char (subs string (inc position))))

(defn create-unsolved-labyrinth [input]
  (let [maze-cells (vec (for [x (range (* 2 (count input)))]
                          (vec
                           (for [y (range (* 2 (count input)))]
                             (unknown-cell)))))]

    {:player (player-at 0 0 south)
     :maze (assoc-in maze-cells [0 0] (empty-cell))}))

(defn render-player [player maze]
  (let [player-symbol (condp = (:facing player)
                        north "A"
                        south "v"
                        east ">"
                        west "<")
        x (:x player)
        y (:y player)]
    (update-in maze [y] #(replace-char % x player-symbol))))

(defn render-cell [cell]
  (condp = (:type cell)
    :unknown "░"
    :wall "⎕"
    :empty " "))

(defn render-labyrinth [{:keys [maze player] :as labyrinth}]
  (let [rows (vec (for [row maze]
                    (s/join (map render-cell row))))]
    (render-player player rows)))

;; cell retrieval
(defn get-cell-in-front-of-player [{:keys [x y facing] :as player}]
  (condp = facing
    north (assoc player :y (dec y))
    south (assoc player :y (inc y))
    east (assoc player :x (inc x))
    west (assoc player :x (dec x))))

(defn get-cell-to-the-left-of-player [{:keys [x y facing] :as player}]
  (condp = facing
    north (assoc player :x (dec x))
    south (assoc player :x (inc x))
    east (assoc player :y (dec y))
    west (assoc player :y (inc y))))

(defn get-cell-to-the-top-left-of-player [{:keys [facing] :as player}]
  (-> player
      get-cell-in-front-of-player
      get-cell-to-the-left-of-player))

(defn mark-wall-at-player-left [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-left-of-player player)]
    (assoc-in labyrinth [:maze y x] (wall-cell))))

(defn mark-wall-at-player-left-top [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-top-left-of-player player)]
    (assoc-in labyrinth [:maze y x] (wall-cell))))

;; TODO should mark cell in front as empty
(defn move-player-forward [{:keys [player] :as labyrinth}]
  (condp = (:facing player)
    north (update-in labyrinth [:player :y] dec)
    south (update-in labyrinth [:player :y] inc)
    east (update-in labyrinth [:player :x] inc)
    west (update-in labyrinth [:player :x] dec)))

;; turning around
(defn turn-right [{:keys [player] :as labyrinth}]
  (assoc-in labyrinth [:player :facing]
            (condp = (:facing player)
              north east
              east south
              south west
              west north)))

(defn turn-left [labyrinth]
  (-> labyrinth
      turn-right
      turn-right
      turn-right))

;; entire moves
(defn move-forward [labyrinth]
  (-> labyrinth
      mark-wall-at-player-left
      move-player-forward))

(defn move-right [labyrinth]
  (-> labyrinth
      mark-wall-at-player-left
      mark-wall-at-player-left-top
      turn-right
      move-forward))

(defn move-left [labyrinth]
  (-> labyrinth
      turn-left
      move-player-forward))
