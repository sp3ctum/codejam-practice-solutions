(ns cljs.codejam-practice-solutions.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [codejam-practice-solutions.practice.b-always-turn-left :as maze]
            [reagent.core :as reagent :refer [atom]]
            [schema.core :as s :include-macros true]))

(enable-console-print!)

(def AppState {(s/required-key :route) s/Str
               (s/required-key :maze) maze/Labyrinth})

(def app-state (atom {:available-routes ["WWWWWWWLW WLWWWRRWWWWWWWWWW"
                                         "WRWWLWWLWWLWLWRRWRWWWRWWRWLW WWRRWLWLWWLWWLWWRWWRWWLW"
                                         (str "WWLWLWRRWRWWLW"
                                              "WWLWLWWRRWLWLWRRWWLW " ; space
                                              "WRWLWLWRWRWWLWWLWLWRWRWW"
                                              "RRWWLWLWRWLWLWRRWRWLWLWW"
                                              "LWWWRRWWWRWLWLWRRWRWLWLW"
                                              "RRWWLWLWWRRWWLWLWWRRWLWLWW")]
                      :route "WWWWWWWLW WLWWWRRWWWWWWWWWW"
                      :labyrinth nil}))

(defn main [])

(s/defn maze-pos->canvas-pos :- s/Num [n]
  (let [margin-from-edge 10
        width 10]
    (+ margin-from-edge
       (* width n))))
(map maze-pos->canvas-pos (range 10))

(s/defn draw-player [{:keys [x y]} :- maze/Player]
  (q/rect (+ 2 (maze-pos->canvas-pos x))
          (+ 2 (maze-pos->canvas-pos y))
          ;; heigt width rounding
          6 6 10))

(s/defn draw-cell [[row column cell] :- maze/Cell]
  (when (= cell
           maze/wall-cell)
    (q/rect (maze-pos->canvas-pos column)
            (maze-pos->canvas-pos row)
            10 10)))

(s/defn draw-cells [rows :- [maze/Row]]
  (doall (map draw-cell
              (maze/get-cells-with-coordinates rows))))

(s/defn draw [labyrinth :- maze/Labyrinth]
  (q/background 200 200 200)
  (draw-player (:player labyrinth))
  (draw-cells (get-in labyrinth [:maze])))

(s/defn change-route [app-state
                      new-route :- s/Str]
  (swap! app-state assoc-in [:route] new-route)
  (swap! app-state update-in [:labyrinth]
         #(maze/solve-debug new-route)))

(defn setup [app-state]
  (change-route app-state (:route @app-state))
  (q/frame-rate 1))

(defn main-component []
  [:div
   [:canvas#canvas]
   [:p "Route"
    [:select
     {:on-change #(change-route app-state
                                (.. % -currentTarget -value))}
     (for [route (get-in @app-state [:available-routes])]
       [:option route])]]])

(reagent/render main-component (js/document.getElementById "app"))

(q/defsketch my-sketch
  :host "canvas"
  :setup #(setup app-state)
  :draw #(draw (:labyrinth @app-state))
  :size [300 300])
