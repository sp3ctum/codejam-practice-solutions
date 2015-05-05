(ns codejam-practice-solutions.practice.b-always-turn-left-test
  (:require [codejam-practice-solutions.practice.b-always-turn-left
             :as maze]
            [clojure.string :as s]
            [clojure.test :refer [deftest is] :as test]))

(declare parse-maze)

(deftest parse-input
  (is (= {:forward "WWWWWWWWWLW", :backward "WRWWWWWWWWW"}
         (maze/parse-input "WWWWWWWWWLW WRWWWWWWWWW"))))

(deftest create-unsolved-labyrinth
  (is (= (parse-maze ["░⎕v⎕░"
                      "░░░░░"
                      "░░░░░"
                      "░░░░░"
                      "░░░░░"])
         (maze/create-unsolved-labyrinth "WW"))))

(deftest replace-char
  (is (= "Abc" (maze/replace-char "abc" 0 "A")))
  (is (= "aBc" (maze/replace-char "abc" 1 "B")))
  (is (= "abC" (maze/replace-char "abc" 2 "C"))))

(deftest render-labyrinth
  (is (= ["░⎕v⎕░"
          "░░░░░"
          "░░░░░"
          "░░░░░"
          "░░░░░"]
         (maze/render-labyrinth (maze/create-unsolved-labyrinth "WW"))))
  (is (= ["░░░░"
          "░v░░"
          "░░░░"
          "░░░░"]
         (maze/render-labyrinth
          (parse-maze ["░░░░"
                       "░v░░"
                       "░░░░"
                       "░░░░"])))))

(defn parse-cell [cell-string]
  (condp = (str cell-string)
    "░" (maze/unknown-cell)
    "⎕" (maze/wall-cell)
    " " (maze/empty-cell)
    "A" (maze/empty-cell)
    "v" (maze/empty-cell)
    "<" (maze/empty-cell)
    ">" (maze/empty-cell)))

(defn parse-player-position [maze]
  (let [[column row cell] (first (for [[row-index row] (map vector (range) maze)
                                       [cell-index cell] (map vector (range) row)
                                       :when (some #(when (= % cell) %) "<A>v")]
                                   [cell-index row-index cell]))]
    (condp = cell
      \A (maze/player-at row column maze/north)
      \v (maze/player-at row column maze/south)
      \< (maze/player-at row column maze/west)
      \> (maze/player-at row column maze/east)
      nil)))

(deftest parse-player-position-test
  (is (= {:x 1, :y 1, :facing :south}
         (parse-player-position ["░░░░"
                                 "░v░░"
                                 "░░░░"
                                 "░░░░"])))
  (is (= {:x 2, :y 2, :facing :north}
         (parse-player-position ["░░░░"
                                 "░░░░"
                                 "░░A░"
                                 "░░░░"])))
  (is (= {:x 1, :y 0, :facing :west}
         (parse-player-position ["░<░░"])))
  (is (= {:x 3, :y 0, :facing :east}
         (parse-player-position ["░░░>"]))))

;; test readability helper :|
(defn parse-maze [lines]
  (let [maze (vec (for [line lines]
                    (vec (map parse-cell line))))
        player-position (parse-player-position lines)]
    {:maze maze
     :player player-position}))

(deftest parse-maze-test
  (is (= {:maze [[{:type :unknown} {:type :unknown} {:type :unknown} {:type :unknown}]
                 [{:type :unknown} {:type :empty} {:type :unknown} {:type :unknown}]],
          :player {:x 1, :y 1, :facing :south}}
         (parse-maze ["░░░░"
                      "░v░░"]))))

(deftest mark-wall-at-player-left
  (is (= (maze/render-labyrinth (maze/mark-wall-at-player-left
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"])))

(deftest move-forward
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░A░░"
                                              "░░░░"
                                              "░░░░"]))) ["░A░░"
                                                          "⎕ ░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕░░"
                                                          "░ >░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░v░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░ ⎕░"
                                                          "░v░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░<░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "< ░░"
                                                          "░⎕░░"
                                                          "░░░░"]))
  ;; marks cell in front as empty
  (is (= (maze/move-forward
          (parse-maze ["░░░"
                       "░A░"]))
         {:maze
          [[{:type :unknown} {:type :empty} {:type :unknown}]
           [{:type :wall} {:type :empty} {:type :unknown}]],
          :player {:y 0, :facing :north, :x 1}})))

(deftest turn-right
  (is (= (maze/render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░<░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░A░░"
                                                          "⎕⎕░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░A░░"
                                              "░░░░"
                                              "░░░░"]))) ["⎕░░░"
                                                          "⎕>░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕⎕░"
                                                          "░v░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░v░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░<⎕░"
                                                          "░░⎕░"
                                                          "░░░░"])))

(deftest turn-left
  (is (= (maze/render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░A░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░<░░"
                                                   "░░░░"
                                                   "░░░░"]))
  (is (= (maze/render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░>░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░A░░"
                                                   "░░░░"
                                                   "░░░░"]))
  (is (= (maze/render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░v░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░>░░"
                                                   "░░░░"
                                                   "░░░░"]))
  (is (= (maze/render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░<░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░v░░"
                                                   "░░░░"
                                                   "░░░░"])))

(deftest move-route
  (is (= ["░⎕ ⎕░"
          "░░ ⎕░"
          "░░v░░"
          "░░░░░"
          "░░░░░"]
         (maze/render-labyrinth
          (maze/move-route
           (maze/create-unsolved-labyrinth "WW")
           "WW")))))

(deftest move-route-and-back
  (is (= ["░⎕A⎕░"
          "░⎕ ⎕░"
          "░⎕ ⎕░"
          "░░░░░"
          "░░░░░"]
         (maze/render-labyrinth
          (maze/move-route-and-back (maze/parse-input "WW WW"))))))

(deftest longest
  (is (= [[3 "abc"] [2 "ab"]] (maze/longest "abc" "ab"))))
