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
  (is (= (parse-maze ["v░░░"
                      "░░░░"
                      "░░░░"
                      "░░░░"])
         (maze/create-unsolved-labyrinth "WW"))))

(deftest replace-char
  (is (= "Abc" (maze/replace-char "abc" 0 "A")))
  (is (= "aBc" (maze/replace-char "abc" 1 "B")))
  (is (= "abC" (maze/replace-char "abc" 2 "C"))))

(deftest render-labyrinth
  (is (= ["v░░░"
          "░░░░"
          "░░░░"
          "░░░░"]
         (maze/render-labyrinth (maze/create-unsolved-labyrinth "WW")))))

(deftest mark-wall-at-player-left
  (is (= {:player {:x 1, :y 1, :facing :south},
          :maze [[{:type :unknown} {:type :unknown}]
                 [{:type :wall}    {:type :unknown}]
                 [{:type :unknown} {:type :unknown}]
                 [{:type :unknown} {:type :unknown}]]}
         (maze/mark-wall-at-player-left
          {:player {:x 1, :y 1, :facing :south},
           :maze [[{:type :unknown} {:type :unknown}]
                  [{:type :unknown} {:type :unknown}]
                  [{:type :unknown} {:type :unknown}]
                  [{:type :unknown} {:type :unknown}]]}))))

(deftest move-player-forward

  (is (= {:player {:x 1, :y 2, :facing :north}}
         (maze/move-player-forward {:player {:x 1, :y 1, :facing maze/north}})))

  (is (= {:player {:x 1, :y 0, :facing :south}}
         (maze/move-player-forward {:player {:x 1, :y 1, :facing maze/south}})))

  (is (= {:player {:x 2, :y 1, :facing :east}}
         (maze/move-player-forward {:player {:x 1, :y 1, :facing maze/east}})))

  (is (= {:player {:x 0, :y 1, :facing :west}}
         (maze/move-player-forward {:player {:x 1, :y 1, :facing maze/west}}))))

(defn parse-cell [cell-string]
  (condp = (str cell-string)
    "░" (maze/unknown-cell)
    "⎕" (maze/wall-cell)
    " " (maze/empty-cell)
    "A" (maze/empty-cell)
    "v" (maze/empty-cell)
    "<" (maze/empty-cell)
    ">" (maze/empty-cell)))

(some #(when (= \l %) %) "alat")

(defn parse-player-position [maze]
  (let [[x y cell] (first (for [[row-index row] (map vector (range) maze)
                                [cell-index cell] (map vector (range) row)
                                :when (some #(when (= % cell) %) "<A>v")]
                            [row-index cell-index cell]))]
    (condp = cell
      \A (maze/player-at x y maze/north)
      \v (maze/player-at x y maze/south)
      \< (maze/player-at x y maze/west)
      \> (maze/player-at x y maze/east)
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
  (is (= {:x 0, :y 1, :facing :west}
         (parse-player-position ["░<░░"])))
  (is (= {:x 0, :y 3, :facing :east}
         (parse-player-position ["░░░>"]))))

;; test readability helper :|
(defn parse-maze [lines]
  (let [maze (for [line lines]
               (map parse-cell line))
        player-position (parse-player-position lines)]
    {:maze maze
     :player player-position}))

(deftest parse-maze-test
  (is (= {:maze [[{:type :unknown} {:type :unknown} {:type :unknown} {:type :unknown}]
                 [{:type :unknown} {:type :empty} {:type :unknown} {:type :unknown}]],
          :player {:x 1, :y 1, :facing :south}}
         (parse-maze ["░░░░"
                      "░v░░"]))))

(deftest mark-wall-at-player-front
  (is (= {:player {:x 1, :y 1, :facing :south},
          :maze (parse-maze ["░⎕░░"
                             "░v░░"
                             "░░░░"
                             "░░░░"])}
         (maze/mark-wall-at-player-front
          {:player {:x 1, :y 1, :facing :south},
           :maze (parse-maze ["░░░░"
                              "░v░░"
                              "░░░░"
                              "░░░░"])}))))
