;; Dashboard - Practice Problems - Google Code Jam
;; https://code.google.com/codejam/contest/32003/dashboard#s=p1
;; Problem B. Always Turn Left
(ns codejam-practice-solutions.practice.b-always-turn-left
  (:require [clojure.string :as s]))

(def forward \W) ; w for walk
(def right \R)
(def left \L)

(def unknown-cell {:type :unknown})
(def wall-cell {:type :wall})
(def empty-cell {:type :empty})

(def south :south)
(def north :north)
(def east :east)
(def west :west)

(def input-small
  (drop 1
        (s/split-lines
         (slurp "./src/clj/codejam_practice_solutions/practice/B-small-practice.in"))))

(def input-large
  (drop 1
        (s/split-lines
         (slurp "./src/clj/codejam_practice_solutions/practice/B-large-practice.in"))))

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
  (let [height (inc (* 2 (count route)))
        width (+ 3 (* 4 (- (count route)
                           2)))
        maze-cells (vec (for [row (range height)]
                          (vec
                           (for [column (range width)]
                             unknown-cell))))
        ;; Player coordinates are always at the top center.
        ;; Adjust to 0-based array index with dec.
        [row column] [0 (dec (quot (inc width)
                                   2))]]

    {:player (player-at row column south)
     :maze (-> maze-cells
               ;; ahead of the player are always walls on both sides
               (assoc-in [1 (dec column)] wall-cell)
               (assoc-in [1 (inc column)] wall-cell)

               ;; On the sides of the player are always empty cells
               ;; to prevent them from being marked as walls.
               ;;
               ;; This would mess up reporting the results since the
               ;; cells next to the player in the beginning are not
               ;; included in the result by specification.
               (assoc-in [0 (dec column)] empty-cell)
               (assoc-in [0 (inc column)] empty-cell)

               ;; under and in front of the player are empty-cells
               (assoc-in [row column] empty-cell)
               (assoc-in [1 column] empty-cell))}))

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
        (vec (filter (fn [cells]
                       (not (every? #(or (= (:type %) :unknown)
                                         (= (:type %) :empty))
                                    cells)))
                     maze))]
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
        new-maze (vec (map (fn [row] (vec (take (- row-length
                                                   latest-column
                                                   earliest-column)
                                                (drop earliest-column row))))
                           maze))
        new-player (update-in player [:x] - earliest-column)]
    (-> labyrinth
        (assoc :maze new-maze)
        (assoc :player new-player))))

(defn compress-labyrinth [labyrinth]
  (->> labyrinth
       non-unknown-rows
       only-non-unknown-columns))

(defn render-labyrinth
  "Render a debug version of the labyrinth. Draws the player and all
  cells present in the labyrinth without removing anything."
  [{:keys [maze player] :as labyrinth}]
  (let [rows (vec (for [row maze]
                    (s/join (map render-cell row))))]
    (render-player player rows)))

(defn render-compressed-labyrinth
  "Used to render a problem result set. Removes extra padding in the
  labyrinth, and does not render the player. If you want raw data, use
  render-labyrinth instead."
  [labyrinth]
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
      (assoc-in labyrinth [:maze y x] wall-cell)
      labyrinth)))

(defn mark-unknown-as-empty-at-player-left [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-left-of-player player)
        cell (get-in labyrinth [:maze y x])]
    (if (= (:type cell) :unknown)
      (assoc-in labyrinth [:maze y x] empty-cell)
      labyrinth)))

(defn mark-wall-at-player-left-top [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-top-left-of-player player)]
    (assoc-in labyrinth [:maze y x] wall-cell)))

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
        (assoc-in [:maze new-y new-x] empty-cell))))

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
  "Since a labyrinth always ends when turning around, marks
  empty-cells to the sides of the player."
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

(defn get-rooms [maze]
  (->> (get-cells-with-coordinates maze)
       (filter (fn [[row column _cell]]
                 (and (odd? row)
                      (odd? column))))
       (group-by (fn [[column row cell]] column))
       (sort-by (fn [[column row cell]] column))
       ;; [1 [[1 1 {:type :wall}]]] -> skip the group key here
       (map second)))

(defn is-empty-cell [cell]
  (= (:type empty-cell)
     (:type cell)))

(defn can-walk-north [row column cells]
  (is-empty-cell (get-in cells [(dec row) column])))

(defn can-walk-south [row column cells]
  (is-empty-cell (get-in cells [(inc row) column])))

(defn can-walk-east [row column cells]
  (is-empty-cell (get-in cells [row (inc column)])))

(defn can-walk-west [row column cells]
  (is-empty-cell (get-in cells [row (dec column)])))

(defn encode-room [maze [row column _cell]]
  (cond-> 0
          (can-walk-north row column maze) (bit-or 2r0001)
          (can-walk-south row column maze) (bit-or 2r0010)
          (can-walk-west row column maze)  (bit-or 2r0100)
          (can-walk-east row column maze)  (bit-or 2r1000)))

(defn convert-labyrinth-to-solution-format [{:keys [maze] :as labyrinth}]
  (let [rooms (get-rooms maze)]
    (map
     (fn [row-of-rooms]
       (->> row-of-rooms
            (map (partial encode-room maze))
            (map (partial format "%x"))
            s/join))
     rooms)))

(defn solve [line]
  (-> (parse-input line)
      move-route-and-back
      compress-labyrinth
      convert-labyrinth-to-solution-format))

(defn solve-and-print [index input]
  (with-out-str
    (println (str "Case #" (inc index) ":"))
    (doall (map println (solve input)))))

(defn solve-file [file-name file-contents]
  (spit file-name
        (s/join (map-indexed solve-and-print
                             file-contents))))

(comment
  (count (:maze (create-unsolved-labyrinth (:backward (parse-input (nth input-large 3))))))
  (count (:backward (parse-input (nth input-large 3))))

  (solve (nth input-large 5))

  (apply max (map count input-small))
  (apply max (map count input-large))

  (solve-file "input-small-output.txt"
              input-small)
  (solve-file "input-large-output.txt"
              (nth input-large 3)))
