(ns ai
  (:require board
            cell))

(defn random
  "Make a move at a random (valid) position."
  ([brd message] message)
  ([brd]
   (loop [[cell & rest] (shuffle (cell/all-cells brd))]
     (if (board/valid-move? cell brd)
       cell
       (recur rest)))))

(defn score-board
  "Euristic to assign a score to the position."
  [{:keys [turn board counts] :as brd}]
  (+ (if (board/victory? brd) 999999999 0)
     (- (get counts turn)
        (get counts (board/opposite-turn turn)))))

(defn minimax
  "Make a move with the best score on the provided depth"
  ([brd message] message)
  ([{:keys [turn] :as brd}]
   (letfn [(iter [n brd]
             (->> (shuffle (cell/all-cells brd))
                  (filter #(board/valid-move? % brd))
                  (map (fn [move]
                         (let [brd (board/move brd move)
                               brd (if (or (zero? n)
                                           (board/victory? brd))
                                     brd
                                     (last (iter (- n 1) brd)))
                               score (score-board brd)
                               score (if (= turn (:turn brd))
                                       (* score -1)
                                       score)]
                           [move score brd])))
                  (sort #(> (second %1) (second %2)))
                  first))]
     (first (iter 1 brd)))))

#_(game/play-game random minimax)
