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
    (update-in maze [x] #(replace-char % y player-symbol))))

(defn render-cell [cell]
  (condp = (:type cell)
    :unknown "░"
    :wall "⎕"
    :empty " "))

(defn render-labyrinth [{:keys [maze player] :as labyrinth}]
  (let [rows (vec (for [row maze]
                    (s/join (map render-cell row))))]
    (render-player player rows)))

(defn all-cells [{:keys [maze]}]
  (mapcat identity maze))

(defn mark-wall-at-player-left [{:keys [player maze] :as labyrinth}]
  (let [new-maze
        (assoc-in maze [(:x player) (dec (:y player))] (wall-cell))]
    (assoc labyrinth :maze new-maze)))

(defn get-cell-in-front-of-player [{:keys [x y facing]}]
  (condp = facing
    north {:x x, :y (inc y)}
    south {:x x, :y (dec y)}
    east {:x (inc x), :y y}
    west {:x (dec x), :y y}))

(defn mark-wall-at-player-front [{:keys [player maze] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-in-front-of-player player)
        new-maze
        (assoc-in maze [x y] (wall-cell))]
    (assoc labyrinth :maze new-maze)))

(defn move-player-forward [{:keys [player] :as labyrinth}]
  (condp = (:facing player)
    north (update-in labyrinth [:player :y] inc)
    south (update-in labyrinth [:player :y] dec)
    east (update-in labyrinth [:player :x] inc)
    west (update-in labyrinth [:player :x] dec)))

(defn move-forward [maze]
  (-> maze
      mark-wall-at-player-left
      move-player-forward))

(defn turn-right [{:keys [player]}]
  (assoc-in player [:facing] ))

(defn move-right [maze player]
  (-> maze
      mark-wall-at-player-left
      mark-wall-at-player-front
      turn-right))

(defn move-left [maze player])

(defn move [maze direction]
  (condp = direction
    forward (move-forward maze)
    right (move-right maze)
    left (move-left maze)))
