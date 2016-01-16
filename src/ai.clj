(ns ai
  (:require board))

(defn random
  "Make a move at a random (valid) position."
  [{:keys [size] :as brd}]
  (letfn [(next []
            (let [x (+ (rand-int size) 1)
                  y (+ (rand-int size) 1)]
              (if (zero? (rand-int 4))
                [[x (+ x 1)] [y (+ y 1)]]
                [x y])))]
    (loop [cell (next)]
      (if (board/valid-move? cell brd)
        cell
        (recur (next))))))
