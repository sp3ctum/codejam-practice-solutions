(ns codejam-practice-solutions.practice.b-always-turn-left-test
  (:require [codejam-practice-solutions.practice.b-always-turn-left
             :as maze]
            [clojure.string :as s]
            [schema.core :as sc]
            [schema.test]
            [clojure.test :refer [deftest is] :as test]))

(test/use-fixtures :once schema.test/validate-schemas)

(declare parse-maze)

(defn render-player [player maze-row-strings]
  (let [player-symbol (condp = (:facing player)
                        maze/north "A"
                        maze/south "v"
                        maze/east ">"
                        maze/west "<")
        x (:x player)
        y (:y player)]
    (update-in maze-row-strings
               [y]
               #(maze/replace-char % x player-symbol))))

(defn render-cell [cell]
  (condp = (:type cell)
    :unknown "░"
    :wall "⎕"
    :empty " "))

(defn render-labyrinth
  "Render a debug version of the labyrinth. Draws the player and all
  cells present in the labyrinth without removing anything."
  [{:keys [maze player] :as labyrinth}]
  (let [rows (vec (for [row maze]
                    (s/join (map render-cell (:cells row)))))]
    (render-player player rows)))

(defn render-compressed-labyrinth
  "Used to render a problem result set. Removes extra padding in the
  labyrinth, and does not render the player. If you want raw data, use
  render-labyrinth instead."
  [labyrinth]
  (let [compressed-labyrinth (maze/compress-labyrinth labyrinth)
        rendered-maze (vec (for [row (:maze compressed-labyrinth)]
                             (s/join (map render-cell (:cells row)))))]
    rendered-maze))

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
          [{:row-has-wall false, :cells [{:type :empty} {:type :empty} {:type :empty}]}
           {:row-has-wall true,  :cells [{:type :wall} {:type :empty} {:type :wall}]}
           {:row-has-wall false, :cells [{:type :unknown} {:type :unknown} {:type :unknown}]}
           {:row-has-wall false, :cells [{:type :unknown} {:type :unknown} {:type :unknown}]}
           {:row-has-wall false, :cells [{:type :unknown} {:type :unknown} {:type :unknown}]}]}
         (sc/with-fn-validation
           (maze/create-unsolved-labyrinth "WW"))))

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
         (render-labyrinth (maze/create-unsolved-labyrinth "WRWWWW")))))

(deftest render-labyrinth-test
  (is (= [" v "
          "⎕ ⎕"
          "░░░"
          "░░░"
          "░░░"]
         (render-labyrinth (maze/create-unsolved-labyrinth "WW")))))

(deftest render-compressed-labyrinth-test
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
         (render-compressed-labyrinth
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
    "░" maze/unknown-cell
    "⎕" maze/wall-cell
    " " maze/empty-cell
    "A" maze/empty-cell
    "v" maze/empty-cell
    "<" maze/empty-cell
    ">" maze/empty-cell))

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
                    {:row-has-wall (.contains line "⎕")
                     :cells (vec (map parse-cell line))}))
        player-position (parse-player-position lines)]
    {:maze maze
     :player player-position}))

(deftest parse-maze-test
  (is (= {:maze [{:row-has-wall false
                  :cells [{:type :unknown} {:type :unknown} {:type :unknown} {:type :unknown}]}
                 {:row-has-wall true,
                  :cells [{:type :unknown} {:type :empty} {:type :wall} {:type :unknown}]}],
          :player {:x 1, :y 1, :facing :south}}
         (parse-maze ["░░░░"
                      "░v⎕░"]))))

(deftest mark-unknown-as-wall-at-player-left
  (is (= (render-labyrinth (maze/mark-unknown-as-wall-at-player-left
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/mark-unknown-as-wall-at-player-left
                                 (parse-maze ["░ ░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░ ░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"])))

(deftest mark-unknown-as-wall-at-player-left
  (is (= (render-labyrinth (maze/mark-unknown-as-empty-at-player-left
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░ ░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/mark-unknown-as-empty-at-player-left
                                 (parse-maze ["░ ░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░ ░░"
                                                          "░>░░"
                                                          "░░░░"
                                                          "░░░░"])))
(deftest move-forward
  (is (= (render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░░░░"
                                              "░A░░"
                                              "░░░░"]))) ["░A░░"
                                                          "⎕ ░░"
                                                          "⎕ ░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕⎕░"
                                                          "░  >"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/move-forward
                                 (parse-maze ["░░░░"
                                              "░v░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░ ⎕░"
                                                          "░ ⎕░"
                                                          "░v░░"]))
  (is (= (render-labyrinth (maze/move-forward
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
          [{:row-has-wall false, :cells [{:type :unknown} {:type :empty} {:type :unknown}]}
           {:row-has-wall true, :cells [{:type :wall} {:type :empty} {:type :unknown}]}
           {:row-has-wall true, :cells [{:type :wall} {:type :empty} {:type :unknown}]}],
          :player {:x 1, :y 0, :facing :north}})))

(deftest turn-right
  (is (= (render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░<░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░A░░"
                                                          "⎕⎕░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░A░░"
                                              "░░░░"
                                              "░░░░"]))) ["⎕░░░"
                                                          "⎕>░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░>░░"
                                              "░░░░"
                                              "░░░░"]))) ["░⎕⎕░"
                                                          "░v░░"
                                                          "░░░░"
                                                          "░░░░"]))
  (is (= (render-labyrinth (maze/turn-right
                                 (parse-maze ["░░░░"
                                              "░v░░"
                                              "░░░░"
                                              "░░░░"]))) ["░░░░"
                                                          "░<⎕░"
                                                          "░░⎕░"
                                                          "░░░░"])))

(deftest turn-left
  (is (= (render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░A░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░<░░"
                                                   "░░░░"
                                                   "░░░░"]))
  (is (= (render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░>░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░A░░"
                                                   "░░░░"
                                                   "░░░░"]))
  (is (= (render-labyrinth
          (maze/turn-left (parse-maze ["░░░░"
                                       "░v░░"
                                       "░░░░"
                                       "░░░░"]))) ["░░░░"
                                                   "░>░░"
                                                   "░░░░"
                                                   "░░░░"]))
  (is (= (render-labyrinth
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
         (render-labyrinth
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
         (render-labyrinth
          (maze/move-route
           (maze/create-unsolved-labyrinth "WLW")
           "WLW")))))

(defn render-compressed-route [route-and-back & debug?]
  (let [labyrinth
        (-> route-and-back
            maze/parse-input
            maze/move-route-and-back)]
    (if debug?
      (render-labyrinth labyrinth)
      (render-compressed-labyrinth labyrinth))))

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
  (is (= [[0 0 {:type :unknown}]
          [0 1 {:type :unknown}]
          [0 2 {:type :unknown}]
          [1 0 {:type :wall}]
          [1 1 {:type :empty}]
          [1 2 {:type :wall}]
          [2 0 {:type :unknown}]
          [2 1 {:type :unknown}]
          [2 2 {:type :unknown}]]
         (maze/get-cells-with-coordinates
          (:maze
           (parse-maze
            ["░░░"
             "⎕v⎕"
             "░░░"]))))))

(deftest first-non-unknown-index
  (is (= 2
         (maze/first-non-unknown-index
          (map :cells
               (:maze (parse-maze ["░░ A ░░"
                                   "░░⎕ ⎕░░"
                                   "░░⎕ ⎕░░"
                                   "░░⎕ ⎕░░"
                                   "░░   ░░"
                                   "░░░░░░░"
                                   "░░░░░░░"])))))))

(deftest only-non-unknown-columns
  (is (= ["⎕ ⎕"
          "⎕ ⎕"
          "⎕ ⎕"]
         (render-compressed-labyrinth
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
         (render-compressed-labyrinth
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
         (render-compressed-labyrinth
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
                              "⎕⎕⎕⎕⎕⎕⎕"])))))

  (is (= [[[1 1 {:type :empty}]]
          [[3 1 {:type :empty}]]
          [[5 1 {:type :empty}]]
          [[7 1 {:type :empty}]]
          [[9 1 {:type :empty}]]
          [[11 1 {:type :empty}]]
          [[13 1 {:type :empty}]]
          [[15 1 {:type :empty}]]
          [[17 1 {:type :empty}]]
          [[19 1 {:type :empty}]]])
      (maze/get-rooms
       (:maze (parse-maze ["⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕  "
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕ ⎕"
                           "⎕⎕⎕"])))))

(deftest can-walk-tests
  (is (maze/can-walk-north
       1 1
       (vec (map :cells (:maze (parse-maze ["⎕ ⎕"
                                            "⎕ ⎕"
                                            "⎕⎕⎕"]))))))

  (is (maze/can-walk-south
       1 1
       (vec (map :cells (:maze (parse-maze ["⎕⎕⎕"
                                            "⎕  "
                                            "⎕ ⎕"]))))))

  (is (maze/can-walk-west
       1 1
       (vec (map :cells (:maze (parse-maze [""
                                            "   "
                                            "⎕ ⎕"]))))))

  (is (maze/can-walk-east
       1 1
       (vec (map :cells (:maze (parse-maze ["⎕⎕⎕"
                                            "⎕  "
                                            "⎕ ⎕"])))))))

(deftest convert-maze-to-solution-format
  (is (= ["1"]
         (maze/convert-labyrinth-to-solution-format
          (parse-maze ["⎕ ⎕"
                       "⎕ ⎕"
                       "⎕⎕⎕"]))))

  (is (= ["a"]
         (maze/convert-labyrinth-to-solution-format
          (parse-maze ["⎕⎕⎕"
                       "⎕  "
                       "⎕ ⎕"]))))

  (is (= ["1"]
         (maze/convert-labyrinth-to-solution-format
          (parse-maze ["⎕ ⎕"
                       "⎕ ⎕"
                       "⎕⎕⎕"]))))

  (is (= ["ac5"
          "386"
          "9c7"
          "e43"
          "9c5"]
         (maze/convert-labyrinth-to-solution-format
          (parse-maze ["⎕⎕⎕⎕⎕ ⎕"
                       "⎕     ⎕"
                       "⎕ ⎕⎕⎕⎕⎕"
                       "⎕ ⎕   ⎕"
                       "⎕ ⎕⎕⎕ ⎕"
                       "⎕     ⎕"
                       "⎕⎕⎕⎕⎕ ⎕"
                       "    ⎕ ⎕"
                       "⎕ ⎕⎕⎕ ⎕"
                       "⎕     ⎕"
                       "⎕⎕⎕⎕⎕⎕⎕"])))))

(deftest solve
  (is (= ["ac5" "386" "9c7" "e43" "9c5"]
         (maze/solve "WRWWLWWLWWLWLWRRWRWWWRWWRWLW WWRRWLWLWWLWWLWWRWWRWWLW")))

  (is (= ["3" "3" "3" "3" "3" "3" "b" "3" "3" "1"]
         (maze/solve "WWWWWWWLW WLWWWRRWWWWWWWWWW"))))


