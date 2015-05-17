;; Dashboard - Practice Problems - Google Code Jam
;; https://code.google.com/codejam/contest/32003/dashboard#s=p1
;; Problem B. Always Turn Left
(ns codejam-practice-solutions.practice.b-always-turn-left
  (:require [clojure.string :as s]))

(def forward \W) ; w for walk
(def right \R)
(def left \L)

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

(defn player-at [row column facing]
  {:x column, :y row, :facing facing})

(defn replace-char [string position char]
  (str (subs string 0 position) char (subs string (inc position))))

(defn create-unsolved-labyrinth [route]
  (let [maze-cells (vec (for [row (range (inc (* 2 (count route))))]
                          (vec
                           (for [column (range (inc (* 2 (inc (count route)))))]
                             (unknown-cell)))))
        [row column] [0 (inc (count route))]]

    {:player (player-at row column south)
     :maze (-> maze-cells
               ;; ahead of the player are always walls on both sides
               (assoc-in [1 (dec column)] (wall-cell))
               (assoc-in [1 (inc column)] (wall-cell))

               ;; On the sides of the player are always empty cells
               ;; to prevent them from being marked as walls.
               ;;
               ;; This would mess up reporting the results since the
               ;; cells next to the player in the beginning are not
               ;; included in the result by specification.
               (assoc-in [0 (dec column)] (empty-cell))
               (assoc-in [0 (inc column)] (empty-cell))

               ;; under and in front of the player are empty-cells
               (assoc-in [row column] (empty-cell))
               (assoc-in [1 column] (empty-cell)))}))

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

(defn non-unknown-rows [{:keys [maze] :as labyrinth}]
  (let [new-maze
        (filter (fn [cells]
                  (not (every? #(or (= (:type %) :unknown)
                                    (= (:type %) :empty))
                               cells)))
                maze)]
    (assoc labyrinth :maze new-maze)))

(defn get-cells-with-coordinates [maze]
  (for [[row-index row] (map vector (range) maze)
        [cell-index cell] (map vector (range) row)]
    [row-index cell-index cell]))

(defn first-non-unknown-index [maze]
  (let [indices
        (map #(->> %
                   (map vector (range))
                   (some (fn [[column-number cell]]
                           (when (not (or (= :unknown (:type cell))
                                          (= :empty (:type cell))))
                             column-number))))
             maze)]
    (apply min (filter (comp not nil?)
                       indices))))

(defn only-non-unknown-columns [{:keys [maze player] :as labyrinth}]
  (let [row-length (count (first maze))
        earliest-column (first-non-unknown-index maze)
        latest-column (first-non-unknown-index (map reverse maze))
        new-maze (map (fn [row] (take (+ 3 (- latest-column earliest-column))
                                      (drop earliest-column row)))
                      maze)
        new-player (update-in player [:x] - earliest-column)]
    (-> labyrinth
        (assoc :maze new-maze)
        (assoc :player new-player))))

(defn compress-labyrinth [labyrinth]
  (->> labyrinth
       non-unknown-rows
       only-non-unknown-columns))

(defn render-labyrinth [{:keys [maze player] :as labyrinth}]
  (let [rows (vec (for [row maze]
                    (s/join (map render-cell row))))]
    (render-player player rows)))

(defn render-compressed-labyrinth [labyrinth]
  (let [compressed-labyrinth (compress-labyrinth labyrinth)
        rendered-maze (vec (for [row (:maze compressed-labyrinth)]
                             (s/join (map render-cell row))))]
    rendered-maze))

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

(defn mark-unknown-as-wall-at-player-left [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-left-of-player player)
        cell (get-in labyrinth [:maze y x])]
    (if (= (:type cell) :unknown)
      (assoc-in labyrinth [:maze y x] (wall-cell))
      labyrinth)))

(defn mark-unknown-as-empty-at-player-left [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-left-of-player player)
        cell (get-in labyrinth [:maze y x])]
    (if (= (:type cell) :unknown)
      (assoc-in labyrinth [:maze y x] (empty-cell))
      labyrinth)))

(defn mark-wall-at-player-left-top [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-top-left-of-player player)]
    (assoc-in labyrinth [:maze y x] (wall-cell))))

(defn move-player-forward [{:keys [player] :as labyrinth}]
  (let [[x y] [(:x player)
               (:y player)]
        [new-x new-y] (condp = (:facing player)
                        north [x (dec y)]
                        south [x (inc y)]
                        east [(inc x) y]
                        west [(dec x) y])]
    (-> labyrinth
        (assoc-in [:player :x] new-x)
        (assoc-in [:player :y] new-y)
        (assoc-in [:maze new-y new-x] (empty-cell)))))

;; turning around
(defn turn-player-right [{:keys [player] :as labyrinth}]
  (assoc-in labyrinth [:player :facing]
            (condp = (:facing player)
              north east
              east south
              south west
              west north)))

(defn turn-left [labyrinth]
  (-> labyrinth
      turn-player-right
      turn-player-right
      turn-player-right))

(defn move-forward [labyrinth]
  (-> labyrinth
      mark-unknown-as-wall-at-player-left
      move-player-forward
      mark-unknown-as-wall-at-player-left
      move-player-forward))

(defn turn-right [labyrinth]
  (-> labyrinth
      mark-unknown-as-wall-at-player-left
      mark-wall-at-player-left-top
      turn-player-right))

(defn move-route [labyrinth route]
  (reduce (fn [result direction]
            (condp = direction
              forward (move-forward result)
              right (turn-right result)
              left (turn-left result)))
          labyrinth
          (seq route)))

(defn longest [& strings]
  (-> (sort-by first >
               (map (juxt count identity) strings))
      first ; -> e.g. [3 abc"]
      second)) ; -> "abc"

(defn turn-around-at-end-of-labyrinth
  "Since a labyrinth always ends when turning around, marks walls to
  the sides of the player."
  [labyrinth]
  (-> labyrinth
      mark-unknown-as-empty-at-player-left
      turn-player-right
      turn-player-right
      mark-unknown-as-empty-at-player-left))

(defn move-route-and-back [{:keys [forward backward]}]
  (-> (create-unsolved-labyrinth (longest forward backward))
      (move-route forward)
      turn-around-at-end-of-labyrinth
      (move-route backward)))
