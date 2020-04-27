(ns conways-game-of-life.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :refer [difference union]]))
(def map-size 50)
(def pixel-size 10)

(defn get-surrounding-cells [cell]
  [(- cell (inc map-size)) (- cell map-size) (- cell (dec map-size))
   (dec cell) (inc cell)
   (+ cell (dec map-size)) (+ cell map-size) (+ cell (inc map-size))])

(defn get-surrounding-living-cells [cell living-cells]
  (let [surrounding-cells (get-surrounding-cells cell)]
  (filter #(contains? living-cells %)
          surrounding-cells)))

(defn get-surrounding-dead-cells [cell living-cells]
  (let [surrounding-cells (get-surrounding-cells cell)
        dead-cells (filter #(not (contains? living-cells %))
                           surrounding-cells)]
    (filter #(or (zero? %) (pos? %)) (set dead-cells))))

(defn get-new-map [old-map]
  (let [dead-cells (mapcat set (map #(get-surrounding-dead-cells % old-map) old-map))]
    (difference
     (union
      (set (map #(if (or (= 2 (count (get-surrounding-living-cells % old-map)))
                         (= 3 (count (get-surrounding-living-cells % old-map)))) % false) old-map))
      (set (map #(if (= 3 (count (get-surrounding-living-cells % old-map))) % false) dead-cells)))
     #{false})))

(defn cell-to-1d [live-cell]
  (+ (* map-size (first live-cell))
     (second live-cell)))

(defn get-initial-map []
  (let [live-cells [[1 25]
                    [2 23] [2 25]
                    [3 13] [3 14] [3 21] [3 22] [3 35] [3 36]
                    [4 12] [4 16] [4 21] [4 22] [4 35] [4 36]
                    [5 1] [5 2] [5 11] [5 17] [5 21] [5 22]
                    [6 1] [6 2] [6 11] [6 15] [6 17] [6 18] [6 23] [6 25]
                    [7 11] [7 17] [7 25]
                    [8 12] [8 16]
                    [9 13] [9 14]]]
    (set (map #(cell-to-1d %) live-cells))))

(defn cell-to-2d [cell]
  {:x (mod cell map-size) :y (quot cell map-size)})

(defn draw-state [state]
  (q/background 240)
  (q/fill (:color state) 255 255)
  (doseq [cell (map #(cell-to-2d %) (:conway-map state))]
    (q/rect (* pixel-size (cell :x))
            (* pixel-size (cell :y))
            pixel-size
            pixel-size)))

(defn setup []
  (q/frame-rate 15)
  (q/color-mode :hsb)
  {:color 0
   :conway-map (get-initial-map)})

(defn update-state [state]
  {:color (mod (+ (:color state) 0.7) 255)
   :conway-map (get-new-map (:conway-map state))})

(q/defsketch conways-game-of-life
  :title "Conway's game of life"
  :size [(* pixel-size map-size) (* pixel-size map-size)]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
