(ns gui
  (:use [seesaw core graphics])
  (:require board
            cell))

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

(defn new-game []
  (let [f (frame :title "Onyx"
                :minimum-size [625 :by 655]
                :content (canvas :id :board
                                 :background "#AACCAA"
                                 :paint #(draw-board! %2
                                                      (board/starting-board)
                                                      {:scale 50})))]))
