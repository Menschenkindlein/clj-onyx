(ns gui
  (:use [seesaw core graphics])
  (:require board
            cell
            game
            ai))

(native!)

(def stone-size 0.2)

(def tg15 0.26795)

(defn coordinates
  "Given a cell, return its real coordinates on the plane."
  [cell brd {:keys [scale] :as config}]
  (if (cell/square-center? cell)
    ;; an average of corners coordinates
    (mapv #(int (/ % 4))
          (reduce (fn [[x1 y1] [x2 y2]]
                    [(+ x1 x2) (+ y1 y2)])
                  (map #(coordinates % brd config)
                       (cell/neighbours cell brd))))
    (let [[x y] cell
          diff (* scale tg15)
          x+ (* (- x 1) scale)
          y+ (* (- y 1) scale)
          offset (int (/ scale 2))]
      [(+ (int (if (odd? y) x+ (+ x+ diff)))
          offset)
       (+ (int (if (odd? x) y+ (+ y+ diff)))
          offset)])))

#_(coordinates [1 1] {:size 12} {:scale 50})
#_(coordinates [[2 3] [1 2]] {:size 12} {:scale 50})
#_(coordinates [1 2] {:size 12} {:scale 50})

(defn draw-stone!
  [g cell color brd {:keys [scale] :as config}]
  (when (not (cell/pseudo-node? cell))
    (draw g
          (apply circle
                 (flatten (list (coordinates cell brd config)
                                (int (* scale stone-size)))))
          (style :background (case color
                               :black java.awt.Color/DARK_GRAY
                               :white java.awt.Color/WHITE)
                 :foreground java.awt.Color/BLACK))))

(defn draw-line!
  [g a1 a2]
  (draw g
        (apply line (concat a1 a2))
        (style :foreground java.awt.Color/BLACK)))

(defn draw-square!
  [g a1 a2 a3 a4]
  (draw g
        (polygon a1 a2 a3 a4)
        (style :background java.awt.Color/GRAY
               :foreground java.awt.Color/BLACK)))

(defn draw-cell!
  [g cell brd config]
  (let [cell-coords (coordinates cell brd config)
        [x y] cell
        neighbours
        (map #(coordinates % brd config)
             (filter (if (cell/square-center? cell)
                       (constantly true)
                       #(and (not (cell/pseudo-node? %))
                             (not (cell/square-center? %))
                             (or (> (first %) x)
                                 (> (second %) y))))
                     (cell/neighbours cell brd)))]
    (do
      (when (cell/square-center? cell)
        ;; here we use the fact that neighbours of
        ;; the square center are created in a certain order
        (let [[x1y1 x2y2 x1y2 x2y1] neighbours]
          (draw-square! g x1y1 x1y2 x2y2 x2y1)))
      (doall
       (map #(draw-line! g cell-coords %) neighbours)))))

(defn draw-board!
  [g {:keys [size board] :as brd} config]
  (do
    (doall
     (map #(draw-cell! g % brd config)
          (for [x (range 1 (+ size 1))
                y (range 1 (+ size 1))]
            [x y])))
    (doall
     (map #(draw-cell! g % brd config)
          (filter
           #(cell/valid-cell? % brd)
           (for [x (range 1 size)
                 y (range 1 size)]
             [[x (+ x 1)] [y (+ y 1)]]))))
    (doall
     (map #(draw-stone! g (first %) (second %) brd config)
          board))))

#_(def f (frame :title "Onyx"
                :minimum-size [625 :by 655]
                :content (canvas :id :board
                                 :background "#AACCAA"
                                 :paint #(draw-board! %2
                                                      (board/starting-board)
                                                      {:scale 50}))))

#_(-> f pack! show!)

#_(-> f
      (select [:#board])
      (config! :paint #(draw-board! %2
                                    (board/starting-board)
                                    {:scale 50})))

(defn square [x] (* x x))

(defn from-coords
  [[x0 y0 :as cell] brd {:keys [scale] :as config}]
  (let [diff (int (/ scale 2))
        [x y] (mapv #(+ (int (/ (- % diff)
                                scale))
                        1)
                    cell)]
    (->> (concat
          (for [x1 (range (- x 1) (+ x 2))
                y1 (range (- y 1) (+ y 2))]
            [x1 y1])
          (for [x1 (range (- x 1) (+ x 2))
                y1 (range (- y 1) (+ y 2))]
            [[x1 (+ x1 1)] [y1 (+ y1 1)]]))
         (filter #(cell/valid-cell? % brd))
         (map #(vector %
                       (let [[x2 y2] (coordinates % brd config)]
                         (+ (square (- x0 x2))
                            (square (- y0 y2))))))
         (sort #(< (second %1) (second %2)))
         first
         first)))

#_(from-coords (coordinates [7 4] (board/starting-board) {:scale 50})
               (board/starting-board)
               {:scale 50})

(defn click-reader
  [brd config f move]
  (let [listener
        (-> f
            (select [:#board])
            (listen :mouse-clicked
                    #(let [mv (from-coords [(.getX %) (.getY %)]
                                           brd
                                           config)]
                       (if (board/valid-move? mv brd)
                         (deliver move mv)
                         (alert (.getSource %) "Wrong move")))))]
    (future (do (deref move) (listener)))))

(defn make-gui-player
  [player-name player-fn]
  (let [config {:scale 50}
        f (frame :title (str "Onyx: " player-name)
                 :minimum-size [625 :by 655]
                 :content (canvas :id :board
                                  :background "#AACCAA"))]
    (-> f pack! show!)
    (fn [brd]
      (let [move (promise)]
        (future
          (do (-> f
                  (select [:#board])
                  (config! :paint #(draw-board! %2
                                                brd
                                                config)))
              (player-fn brd config f move)))
        (deref move)))))

#_(game/play-game ai/random
                  (make-gui-player
                   "Unsoppable genius"
                   click-reader))
