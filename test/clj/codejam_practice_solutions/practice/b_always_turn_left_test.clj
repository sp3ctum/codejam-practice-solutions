(ns codejam-practice-solutions.practice.b-always-turn-left-test
  (:require [codejam-practice-solutions.practice.b-always-turn-left
             :as maze]
            [clojure.string :as s]
            [clojure.test :refer [deftest is] :as test]))

(declare parse-maze)

(deftest parse-input
  (is (= {:forward "WWWWWWWWWLW", :backward "WRWWWWWWWWW"}
         (maze/parse-input "WWWWWWWWWLW WRWWWWWWWWW"))))

(deftest replace-char
  (is (= "Abc" (maze/replace-char "abc" 0 "A")))
  (is (= "aBc" (maze/replace-char "abc" 1 "B")))
  (is (= "abC" (maze/replace-char "abc" 2 "C"))))

(deftest create-unsolved-labyrinth
  (is (= {:player {:x 1, :y 0, :facing :south},
          :maze
          [[{:type :empty} {:type :empty} {:type :empty}]
           [{:type :wall} {:type :empty} {:type :wall}]
           [{:type :unknown} {:type :unknown} {:type :unknown}]
           [{:type :unknown} {:type :unknown} {:type :unknown}]
           [{:type :unknown} {:type :unknown} {:type :unknown}]]}
         (maze/create-unsolved-labyrinth "WW")))

  (is (= ["░░░░░░░░ v ░░░░░░░░"
          "░░░░░░░░⎕ ⎕░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"
          "░░░░░░░░░░░░░░░░░░░"]
         (maze/render-labyrinth (maze/create-unsolved-labyrinth "WRWWWW")))))

(deftest render-labyrinth
  (is (= [" v "
          "⎕ ⎕"
          "░░░"
          "░░░"
          "░░░"]
         (maze/render-labyrinth (maze/create-unsolved-labyrinth "WW")))))

(deftest render-compressed-labyrinth
  (is (= ["⎕⎕⎕⎕⎕ ⎕"
          "⎕     ⎕"
          "⎕ ⎕⎕⎕⎕⎕"
          "⎕ ⎕   ⎕"
          "⎕ ⎕⎕⎕ ⎕"
          "⎕     ⎕"
          "⎕⎕⎕⎕⎕ ⎕"
          "    ⎕ ⎕"
          "⎕ ⎕⎕⎕ ⎕"
          "⎕     ⎕"
          "⎕⎕⎕⎕⎕⎕⎕"]
         (maze/render-compressed-labyrinth
          (parse-maze ["░░░░░░░░░░░░░░░A ░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕⎕⎕⎕⎕ ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕     ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕ ⎕⎕⎕⎕⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕ ⎕   ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕ ⎕⎕⎕ ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕     ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░ ⎕⎕⎕⎕⎕ ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░     ⎕ ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░ ⎕ ⎕⎕⎕ ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕     ⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░⎕⎕⎕⎕⎕⎕⎕░░░░░░░░░░░░░░"
                       "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
                       "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
                       "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"])))))

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

(deftest mark-unknown-as-wall-at-player-left
  (is (= (maze/render-labyrinth (maze/mark-unknown-as-wall-at-player-left
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/mark-unknown-as-wall-at-player-left
                                 (parse-maze ["░ ░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░ ░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"])))

(deftest mark-unknown-as-wall-at-player-left
  (is (= (maze/render-labyrinth (maze/mark-unknown-as-empty-at-player-left
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░ ░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/mark-unknown-as-empty-at-player-left
                                 (parse-maze ["░ ░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░ ░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"])))
(deftest move-forward
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░░░░"
                                              "░A░░"
                                              "░░░░"]))) ["░A░░"
                                                          "⎕ ░░"
                                                          "⎕ ░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕⎕░"
                                                          "░  >"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░v░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░ ⎕░"
                                                          "░ ⎕░"
                                                          "░v░░"]))
  (is (= (maze/render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░░<░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "<  ░"
                                                          "░⎕⎕░"
                                                          "░░░░"]))
  ;; marks cell in front as empty
  (is (= (maze/move-forward
          (parse-maze ["░░░"
                       "░░░"
                       "░A░"]))
         {:maze
          [[{:type :unknown} {:type :empty} {:type :unknown}]
           [{:type :wall} {:type :empty} {:type :unknown}]
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
  (is (= ["   "
          "⎕ ⎕"
          "░ ⎕"
          "░ ⎕"
          "░v░"]
         (maze/render-labyrinth
          (maze/move-route
           (maze/create-unsolved-labyrinth "WW")
           "WW"))))
  (is (= ["░░   ░░"
          "░░⎕ ⎕░░"
          "░░░  >░"
          "░░░░░░░"
          "░░░░░░░"
          "░░░░░░░"
          "░░░░░░░"]
         (maze/render-labyrinth
          (maze/move-route
           (maze/create-unsolved-labyrinth "WLW")
           "WLW")))))

(defn render-compressed-route [route-and-back & debug?]
  (let [labyrinth
        (-> route-and-back
            maze/parse-input
            maze/move-route-and-back)]
    (if debug?
      (maze/render-labyrinth labyrinth)
      (maze/render-compressed-labyrinth labyrinth))))

(deftest move-route-and-back
  (is (= ["⎕ ⎕"
          "⎕ ⎕"
          "⎕ ⎕"]
         (render-compressed-route "WW WW")))

  (is (= ["⎕⎕⎕⎕⎕ ⎕"
          "⎕     ⎕"
          "⎕ ⎕⎕⎕⎕⎕"
          "⎕ ⎕   ⎕"
          "⎕ ⎕⎕⎕ ⎕"
          "⎕     ⎕"
          "⎕⎕⎕⎕⎕ ⎕"
          "    ⎕ ⎕"
          "⎕ ⎕⎕⎕ ⎕"
          "⎕     ⎕"
          "⎕⎕⎕⎕⎕⎕⎕"]
         (render-compressed-route
          "WRWWLWWLWWLWLWRRWRWWWRWWRWLW WWRRWLWLWWLWWLWWRWWRWWLW")))

  (is (= ["⎕⎕⎕⎕⎕ ⎕ ⎕⎕⎕"
          "⎕     ⎕   ⎕"
          "⎕⎕⎕ ⎕ ⎕⎕⎕ ⎕"
          "⎕   ⎕ ⎕ ⎕ ⎕"
          "⎕ ⎕ ⎕ ⎕ ⎕ ⎕"
          "⎕ ⎕ ⎕     ⎕"
          "⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕"]
         (render-compressed-route
          "WWWLWLWRRWLWLWWLWRW WLWRWWRWWRWWLWLWWRRWLWLWRRWRWLWLWRRWWLW")))

  (is (= ["⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕ ⎕⎕⎕"
          "⎕   ⎕ ⎕ ⎕ ⎕ ⎕ ⎕"
          "⎕⎕⎕ ⎕ ⎕ ⎕ ⎕ ⎕ ⎕"
          "⎕     ⎕ ⎕     ⎕"
          "⎕ ⎕⎕⎕ ⎕ ⎕ ⎕⎕⎕⎕⎕"
          "⎕ ⎕       ⎕ ⎕ ⎕"
          "⎕ ⎕ ⎕ ⎕⎕⎕ ⎕ ⎕ ⎕"
          "⎕ ⎕ ⎕   ⎕ ⎕   ⎕"
          "⎕ ⎕⎕⎕⎕⎕ ⎕ ⎕ ⎕ ⎕"
          "⎕ ⎕     ⎕   ⎕  "
          "⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕⎕"]
         (render-compressed-route (str "WWLWLWRRWRWWLW"
                                       "WWLWLWWRRWLWLWRRWWLW " ; space
                                       "WRWLWLWRWRWWLWWLWLWRWRWW"
                                       "RRWWLWLWRWLWLWRRWRWLWLWW"
                                       "LWWWRRWWWRWLWLWRRWRWLWLW"
                                       "RRWWLWLWWRRWWLWLWWRRWLWLWW")))))

;; for testing output by skimming the results manually in order to
;; locate more problems
(defn solve-with-debug-data [problem-number route]
  #_(println "solving problem" problem-number
             "with route" route)
  [problem-number
   route
   (render-compressed-route route)])

(comment
  (map-indexed #(solve-with-debug-data (inc %1) %2)
               (drop 1 maze/input-small)))

(deftest longest
  (is (= "abc" (maze/longest "abc" "ab"))))

(deftest non-unknown-row
  (is (= (count (:maze (maze/non-unknown-rows
                        (parse-maze ["░   ░"
                                     "░⎕ ⎕░"
                                     "░⎕ ⎕░"
                                     "░⎕ ⎕░"
                                     "░⎕ ⎕░"
                                     "░⎕ ⎕░"
                                     "░   ░"
                                     "░░░░░"]))))
         5)))

(deftest get-cells-with-coordinates
  (is (= [[0 0 \░] [0 1 \░] [0 2 \░]
          [1 0 \⎕] [1 1 \v] [1 2 \⎕]
          [2 0 \░] [2 1 \░] [2 2 \░]]
         (maze/get-cells-with-coordinates
          ["░░░"
           "⎕v⎕"
           "░░░"]))))

(deftest first-non-unknown-index
  (is (= 2
         (maze/first-non-unknown-index
          (:maze (parse-maze ["░░ A ░░"
                              "░░⎕ ⎕░░"
                              "░░⎕ ⎕░░"
                              "░░⎕ ⎕░░"
                              "░░   ░░"
                              "░░░░░░░"
                              "░░░░░░░"]))))))

(deftest only-non-unknown-columns
  (is (= ["⎕ ⎕"
          "⎕ ⎕"
          "⎕ ⎕"]
         (maze/render-compressed-labyrinth
          (maze/only-non-unknown-columns
           (parse-maze ["░░ A ░░"
                        "░░⎕ ⎕░░"
                        "░░⎕ ⎕░░"
                        "░░⎕ ⎕░░"
                        "░░   ░░"]))))))

(deftest non-unknown-rows
  (is (= ["⎕ ⎕"
          "⎕ ⎕"
          "⎕ ⎕"]
         (maze/render-compressed-labyrinth
          (maze/non-unknown-rows
           (parse-maze [" A " ; player will be ignored
                        "⎕ ⎕"
                        "⎕ ⎕"
                        "⎕ ⎕"
                        "   "
                        "░░░"]))))))

(deftest compress-maze
  (is (= ["⎕ ⎕"
          "⎕ ⎕"
          "⎕ ⎕"]
         (maze/render-compressed-labyrinth
          (maze/compress-labyrinth
           (parse-maze ["░░ A ░░"
                        "░░⎕ ⎕░░"
                        "░░⎕ ⎕░░"
                        "░░⎕ ⎕░░"
                        "░░   ░░"
                        "░░░░░░░"
                        "░░░░░░░"]))))))

(deftest get-rooms
  (is (= [[[1 1 {:type :empty}] [1 3 {:type :empty}] [1 5 {:type :empty}]]
          [[3 1 {:type :empty}] [3 3 {:type :empty}] [3 5 {:type :empty}]]
          [[5 1 {:type :empty}] [5 3 {:type :empty}] [5 5 {:type :empty}]]
          [[7 1 {:type :empty}] [7 3 {:type :empty}] [7 5 {:type :empty}]]
          [[9 1 {:type :empty}] [9 3 {:type :empty}] [9 5 {:type :empty}]]]
         (maze/get-rooms
          (:maze (parse-maze ["⎕⎕⎕⎕⎕ ⎕"
                              "⎕     ⎕"
                              "⎕ ⎕⎕⎕⎕⎕"
                              "⎕ ⎕   ⎕"
                              "⎕ ⎕⎕⎕ ⎕"
                              "⎕     ⎕"
                              "⎕⎕⎕⎕⎕ ⎕"
                              "    ⎕ ⎕"
                              "⎕ ⎕⎕⎕ ⎕"
                              "⎕     ⎕"
                              "⎕⎕⎕⎕⎕⎕⎕"]))))))

(deftest has-space-tests
  (is (maze/has-space-above 1 1
                            (:maze (parse-maze ["⎕ ⎕"
                                                "⎕ ⎕"
                                                "⎕⎕⎕"]))))
  (is (maze/has-space-below 1 1
                            (:maze (parse-maze ["⎕⎕⎕"
                                                "⎕  "
                                                "⎕ ⎕"]))))

  (is (maze/has-space-left 1 1
                           (:maze (parse-maze ["⎕⎕⎕"
                                               "   "
                                               "⎕ ⎕"]))))

  (is (maze/has-space-right 1 1
                            (:maze (parse-maze ["⎕⎕⎕"
                                                "⎕  "
                                                "⎕ ⎕"])))))

(deftest convert-maze-to-solution-format
  (is (= ["1"]
         (maze/convert-maze-to-solution-format
          (:maze (parse-maze ["⎕ ⎕"
                              "⎕ ⎕"
                              "⎕⎕⎕"])))))

  (is (= ["a"]
         (maze/convert-maze-to-solution-format
          (:maze (parse-maze ["⎕⎕⎕"
                              "⎕  "
                              "⎕ ⎕"])))))

  (is (= ["1"]
         (maze/convert-maze-to-solution-format
          (:maze (parse-maze ["⎕ ⎕"
                              "⎕ ⎕"
                              "⎕⎕⎕"])))))

  (is (= ["ac5"
          "386"
          "9c7"
          "e43"
          "9c5"]
         (maze/convert-maze-to-solution-format
          (:maze (parse-maze ["⎕⎕⎕⎕⎕ ⎕"
                              "⎕     ⎕"
                              "⎕ ⎕⎕⎕⎕⎕"
                              "⎕ ⎕   ⎕"
                              "⎕ ⎕⎕⎕ ⎕"
                              "⎕     ⎕"
                              "⎕⎕⎕⎕⎕ ⎕"
                              "    ⎕ ⎕"
                              "⎕ ⎕⎕⎕ ⎕"
                              "⎕     ⎕"
                              "⎕⎕⎕⎕⎕⎕⎕"]))))))
