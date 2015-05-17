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
  (is (= {:player {:x 2, :y 0, :facing :south},
          :maze
          [[{:type :unknown}
            {:type :empty}
            {:type :empty}
            {:type :empty}
            {:type :unknown}]
           [{:type :unknown}
            {:type :wall}
            {:type :empty}
            {:type :wall}
            {:type :unknown}]
           [{:type :unknown}
            {:type :unknown}
            {:type :unknown}
            {:type :unknown}
            {:type :unknown}]]}
         (maze/create-unsolved-labyrinth "W")))

  (is (= ["░░ v ░░"
          "░░⎕ ⎕░░"
          "░░░░░░░"
          "░░░░░░░"
          "░░░░░░░"]
         (maze/render-labyrinth (maze/create-unsolved-labyrinth "WW")))))

(deftest render-labyrinth
  (is (= ["░░ v ░░"
          "░░⎕ ⎕░░"
          "░░░░░░░"
          "░░░░░░░"
          "░░░░░░░"]
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
  (is (= ["░░   ░░"
          "░░⎕ ⎕░░"
          "░░░ ⎕░░"
          "░░░ ⎕░░"
          "░░░v░░░"]
         (maze/render-labyrinth
          (maze/move-route
           (maze/create-unsolved-labyrinth "WW")
           "WW"))))
  (is (= ["░░░   ░░░"
          "░░░⎕ ⎕░░░"
          "░░░░  >░░"
          "░░░░░░░░░"
          "░░░░░░░░░"
          "░░░░░░░░░"
          "░░░░░░░░░"]
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
(comment
  (map-indexed (fn [i route]
                 [(inc i) route (render-compressed-route route)])
               (drop 1 maze/input-small)))

(deftest longest
  (is (= "abc" (maze/longest "abc" "ab"))))

(deftest non-unknown-row
  (is (= (count (:maze (maze/non-unknown-rows
                        (parse-maze ["░░   ░░"
                                     "░░⎕ ⎕░░"
                                     "░░⎕ ⎕░░"
                                     "░░⎕ ⎕░░"
                                     "░░⎕ ⎕░░"
                                     "░░⎕ ⎕░░"
                                     "░░   ░░"
                                     "░░░░░░░"]))))
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
                        "░░░░░░░"])))))

  (is (= nil
         )))





