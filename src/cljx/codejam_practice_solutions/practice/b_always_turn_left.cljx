;; Dashboard - Practice Problems - Google Code Jam
;; https://code.google.com/codejam/contest/32003/dashboard#s=p1
;; Problem B. Always Turn Left

(ns codejam-practice-solutions.practice.b-always-turn-left
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

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

;; Short algorithm explanation:
;;
;; A labyrinth is given as two routes: the route going forward
;; (start to finish) and backward (finish to start). The way labyrinth
;; solving works is by creating a huge labyrinth that can contain any
;; permutation of the longer route (forward backward), moving
;; a "player" character inside it until both routes are exhausted, and
;; finally compressing the labyrinth by removing extra space around
;; it.
;;
;; How do we know where the walls are when moving?
;; - if turning left, there must be space on the left hand side
;; - if moving forward, there must be space in the front and a wall on
;;   the left
;; - if turning right, we can neither turn left or go forward, so
;;   there must be walls there
;;

(defn parse-input [input-line]
  (let [[entrance-to-exit exit-to-entrance]
        (s/split input-line #" ")]
    {:forward entrance-to-exit
     :backward exit-to-entrance}))

(defn player-at [row column facing]
  {:x column, :y row, :facing facing})

(defn replace-char [string position char]
  (str (subs string 0 position) char (subs string (inc position))))

(defn assoc-cell [labyrinth y x new-cell]
  (if (= new-cell wall-cell)
    (some-> labyrinth
            (assoc-in [:maze y :cells x] new-cell)
            (assoc-in [:maze y :row-has-wall] true))
    (assoc-in labyrinth [:maze y :cells x] new-cell)))

(defn create-unsolved-labyrinth
  "Create a labyrinth that is large enough to contain any permutation
  of the given route. This labyrinth will be absolutely huge for large
  routes (above a couple hundred moves), so it's best not to render it
  from the repl. If you want to see what it looks like, you can use
  the render-compressed-labyrinth function in the test namespace to
  show only the labyrinth and nothing around it."
  [route]
  (let [height (inc (* 2 (count route)))
        width (+ 3 (* 4 (- (count route)
                           2)))
        maze-rows (p :create-maze-cells
                     (let [row {:row-has-wall false
                                :cells (vec (for [column (range width)]
                                              unknown-cell))}]
                       (vec (repeat height row))))
        ;; Player coordinates are always at the top center.
        ;; Adjust to 0-based array index with dec.
        [row column] [0 (dec (quot (inc width)
                                   2))]
        labyrinth {:player (player-at row column south)
                   :maze maze-rows}]

    (-> labyrinth
        ;; ahead of the player are always walls on both sides
        (assoc-cell 1 (dec column) wall-cell)
        (assoc-cell 1 (inc column) wall-cell)

        ;; On the sides of the player are always empty cells
        ;; to prevent them from being marked as walls.
        ;;
        ;; This would mess up reporting the results since the
        ;; cells next to the player in the beginning are not
        ;; included in the result by specification.
        (assoc-cell 0 (dec column) empty-cell)
        (assoc-cell 0 (inc column) empty-cell)

        ;; under and in front of the player are empty-cells
        (assoc-cell row column empty-cell)
        (assoc-cell 1 column empty-cell))))

(defnp non-unknown-rows [{:keys [maze] :as labyrinth}]
  (let [new-maze (vec (filter :row-has-wall maze))]
    (assoc labyrinth :maze new-maze)))

(defn is-empty-cell [cell]
  (= (:type empty-cell)
     (:type cell)))

(defn first-non-unknown-index [row-cells]
  (let [indices
        (map #(->> %
                   (map vector (range))
                   (some (fn [[column-number cell]]
                           (when (not (or (= :unknown (:type cell))
                                          (= :empty (:type cell))))
                             column-number))))
             row-cells)]
    (apply min (filter (comp not nil?)
                       indices))))

(defnp only-non-unknown-columns [{:keys [maze player] :as labyrinth}]
  (let [row-cells (map :cells maze)
        row-length (count (first row-cells))
        earliest-column (first-non-unknown-index row-cells)
        latest-column (first-non-unknown-index (map reverse row-cells))
        new-maze (map (fn [row]
                        (update-in row [:cells]
                                   #(vec (take (- row-length
                                                  latest-column
                                                  earliest-column)
                                               (drop earliest-column %)))))
                      maze)
        new-player (update-in player [:x] - earliest-column)]
    (-> labyrinth
        (assoc :maze new-maze)
        (assoc :player new-player))))

(defnp compress-labyrinth
  [labyrinth]
  (->> labyrinth
       non-unknown-rows
       only-non-unknown-columns))

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

(defn get-cell-to-the-top-left-of-player [player]
  (-> player
      get-cell-in-front-of-player
      get-cell-to-the-left-of-player))

(defn mark-unknown-as-wall-at-player-left [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-left-of-player player)
        cell (get-in labyrinth [:maze y :cells x])]
    (if (= (:type cell) :unknown)
      (assoc-cell labyrinth y x wall-cell)
      labyrinth)))

(defn mark-unknown-as-empty-at-player-left [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-left-of-player player)
        cell (get-in labyrinth [:maze y :cells x])]
    (if (= (:type cell) :unknown)
      (assoc-cell labyrinth y x empty-cell)
      labyrinth)))

(defn mark-wall-at-player-left-top [{:keys [player] :as labyrinth}]
  (let [{:keys [x y]} (get-cell-to-the-top-left-of-player player)]
    (assoc-cell labyrinth y x wall-cell)))

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
        (assoc-cell new-y new-x empty-cell))))

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

(defnp move-route-and-back [{:keys [forward backward]}]
  (-> (create-unsolved-labyrinth (longest forward backward))
      (move-route forward)
      turn-around-at-end-of-labyrinth
      (move-route backward)))

(defn get-cells-with-coordinates [maze]
  (for [[row-index row] (map vector (range) maze)
        [cell-index cell] (map vector (range) (:cells row))]
    [row-index cell-index cell]))

(defn get-rooms [maze]
  (->> (get-cells-with-coordinates maze)
       (filter (fn [[row column _cell]]
                 (and (odd? row)
                      (odd? column))))
       (group-by (fn [[column row cell]] column))
       (sort-by (fn [[column row cell]] column))
       ;; [1 [[1 1 {:type :wall}]]] -> skip the group key here
       (map second)))

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

(defnp convert-labyrinth-to-solution-format [{:keys [maze] :as labyrinth}]
  (let [row-cells (vec (map :cells maze))
        rooms (get-rooms maze)]
    (map
     (fn [row-of-rooms]
       (->> row-of-rooms
            (map (partial encode-room row-cells))
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
        (s/join (pmap solve-and-print
                      (range)
                      file-contents))))

(def input-small
  (drop 1
        (s/split-lines
         (slurp (io/resource "input-files/practice/B-small-practice.in")))))

(def input-large
  (drop 1
        (s/split-lines
         (slurp (io/resource "input-files/practice/B-large-practice.in")))))

(comment

  (profile :info :whatever
           (solve-file "B-small-practice.out"
                       input-small))

  (profile :info :whatever
           (solve-file "B-large-practice.out"
                       input-large)))
