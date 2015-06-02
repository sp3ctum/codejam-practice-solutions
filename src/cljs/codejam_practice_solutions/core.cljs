(ns cljs.codejam-practice-solutions.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [codejam-practice-solutions.practice.b-always-turn-left :as maze]
            [reagent.core :as reagent :refer [atom]]
            [clojure.string]
            [schema.core :as s :include-macros true]))

(enable-console-print!)

(defonce app-state
  (atom {:selected-route-index 0
         :available-routes ["WWWWWWWLW WLWWWRRWWWWWWWWWW"
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

(defn draw [app-state]
  (q/background 200 200 200)
  (draw-player (get-in @app-state [:labyrinth :player]))
  (draw-cells (get-in @app-state [:labyrinth :maze])))

(defn change-route [app-state new-route-index]
  (let [new-route (nth (:available-routes @app-state)
                       new-route-index)]
    (swap! app-state
           #(some-> %
                    (assoc-in [:route] new-route)
                    (assoc-in [:selected-route-index] new-route-index)
                    (assoc-in [:labyrinth] (maze/solve-debug new-route))))))

(defn setup [app-state]
  (change-route app-state 0)
  (q/frame-rate 2))

(defn read-input-file
  "app-state must be an atom, not a deref'ed thing"
  [input app-state]
  (let [file (aget (. input -files) 0)
        update-available-routes
        #(let [contents (clojure.string/split-lines
                         (.. % -target -result))]
           (print "updating :available-routes to" contents)
           (swap! app-state assoc-in [:available-routes] contents))

        reader (doto (js/FileReader.)
                 (set! -onload update-available-routes)
                 (set! -onerror #(js/alert %)))]
    (.readAsText reader file)))

(defn get-current-route [app-state]
  (nth (:available-routes @app-state)
       (:selected-route-index @app-state)))

(defn get-current-route-in-direction [app-state direction]
  (let [[forward backward] (clojure.string/split
                            (get-current-route app-state) #" ")]
    (case direction
      :forward forward
      :backward backward)))

(defn next-route [app-state]
  (let [routes (:available-routes @app-state)
        new-route-index (min (inc (:selected-route-index
                                   @app-state))
                             (dec (count routes)))]
    (change-route app-state new-route-index)))

(defn previous-route [app-state]
  (let [routes (:available-routes @app-state)
        new-route-index (max (dec (:selected-route-index @app-state))
                             0)]
    (change-route app-state new-route-index)))

(defn input-controls []
  [:div
   [:div.row
    [:div.col-xs-12
     [:p "The goal is to walk through a labyrinth, then walk back, and report what the labyrinth looks like."]
     [:p "Rules for solving:"
      [:ul
       [:li "You get a route from the start to the end of the labyrinth"]
       [:li "You can only turn left"]
       [:li "If you can turn left at any point you must turn"]
       [:li "If you can't turn left you must walk forward"]]]]]
   [:div.row
    [:div.col-xs-2
     [:p "Input file:"]]
    [:div.col-xs-10
     [:input#input-file
      {:type "file"
       :on-change #(this-as me
                            (-> me
                                .getDOMNode
                                (read-input-file app-state)))}]]]
   [:div.row
    [:div.col-xs-12

     [:p
      "Route "
      [:span.glyphicon.glyphicon-chevron-left
       {:on-click #(previous-route app-state)}]
      (inc (:selected-route-index @app-state))
      " / "
      (count (:available-routes @app-state))
      [:span.glyphicon.glyphicon-chevron-right
       {:on-click #(next-route app-state)}]]]]
   [:div.row
    [:div.col-xs-2
     [:p "Length: " (count (get-current-route app-state))]]
    [:div.col-xs-10
     [:div.row [:p "Forward"]]
     [:div.row
      [:textarea {:value (get-current-route-in-direction app-state :forward)
                  :cols 50
                  :read-only true}]]
     [:div.row [:p "Backward"]]
     [:div.row
      [:textarea {:value (get-current-route-in-direction app-state :backward)
                  :cols 50
                  :read-only true}]]]]])

(defn main-component []
  [:div.container
   [:div.jumbotron
    [:p "Visualize a labyrinth"]]
   [:div.row
    [:div.col-xs-5 [:canvas#canvas]]
    [:div.col-xs-7
     [input-controls]]]])

(reagent/render main-component (js/document.getElementById "app"))

(q/defsketch my-sketch
  :host "canvas"
  :setup #(setup app-state)
  :draw #(draw app-state)
  :size [400 400])
