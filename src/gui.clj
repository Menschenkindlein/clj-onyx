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
    (when (cell/square-center? cell)
      ;; here we use the fact that neighbours of
      ;; the square center are created in a certain order
      (let [[x1y1 x2y2 x1y2 x2y1] neighbours]
        (draw-square! g x1y1 x1y2 x2y2 x2y1)))
    (doseq [neighbour neighbours]
      (draw-line! g cell-coords neighbour))))

(defn calculate-config
  "Calculate the optimal distance between cells"
  [g {:keys [size]}]
  { :scale (int (/ (-> (.getSize g)
                       (#(min (.getHeight %) (.getWidth %))))
                   (+ size tg15))) })

(defn draw-board!
  [g {:keys [size board] :as brd}]
  (let [config (calculate-config (.getComponent (.getDestination g)) brd)]
   (doseq [cell (cell/all-cells brd)]
     (draw-cell! g cell brd config))
   (doseq [[cell color] board]
     (draw-stone! g cell color brd config))))

(defn distance-squared
  [[x1 y1] [x2 y2]]
  (letfn [(square [x] (* x x))]
    (+ (square (- x1 x2))
       (square (- y1 y2)))))

(defn from-coords
  "Given a cell coordinates on the plane, return its in-game coordinates."
  [cell brd {:keys [scale] :as config}]
  (->> (cell/all-cells brd)
       (map #(vector % (distance-squared (coordinates % brd config)
                                         cell)))
       (sort #(< (second %1) (second %2)))
       first
       first))

#_(from-coords (coordinates [7 4] (board/starting-board) {:scale 50})
               (board/starting-board)
               {:scale 50})

(defn click-reader
  [f brd]
  (let [config (calculate-config (select f [:#board]) brd)
        move (promise)
        listener (-> f
                     (select [:#board])
                     (listen :mouse-clicked
                             #(let [mv (from-coords [(.getX %) (.getY %)]
                                                    brd
                                                    config)]
                                (if (board/valid-move? mv brd)
                                  (deliver move mv)
                                  (alert (.getSource %) "Wrong move")))))]
    (deref move)
    (listener)
    (deref move)))

(defn make-gui-player
  [player-name player-fn f]
  (fn
    ([brd message]
     (-> f
         (select [:#board])
         (config! :paint #(draw-board! %2 brd)))
     (alert f (str player-name ", " message)))
    ([brd]
     (let [move (do (-> f
                        (config! :title (str "Onyx: " player-name))
                        (select [:#board])
                        (config! :paint #(draw-board! %2 brd)))
                    (player-fn f brd))]
       (-> f
           (config! :title (str "Onyx: " player-name " ...waiting..."))
           (select [:#board])
           (config! :paint #(draw-board! %2 (board/move brd move))))
       move))))

(defn make-game-frame []
  (-> (frame :title "Onyx"
             :minimum-size [625 :by 655]
             :content (canvas :id :board
                              :background "#AACCAA"))
      pack!
      show!))

#_(let [f (make-game-frame)]
    (game/play-game (make-gui-player
                     "Unstoppable genius"
                     click-reader
                     f)
                    (make-gui-player
                     "Wonderful thinker"
                     click-reader
                     f)))
