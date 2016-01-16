(ns game
  (:require board))

(defn play-game
  "Players are functions that, given a board,
   return a next move.
   Note, that the first move of the second player
   may be `board/invalid-move?`. This will mean,
   that that player wants to use the pie rule,
   i.e., switch the colors and pass the right
   of the move to the first player.
   Any other time `board/invalid-move?` means
   immediate defeat of the corresponding player."
  ([player1 player2]
   (play-game player1 player2 12))
  ([player1 player2 size]
   (let [brd (board/starting-board size)
         brd1 (board/move brd (player1 brd))
         p2-first-move (player2 brd1)
         pie? (not (board/valid-move? p2-first-move brd))]
     (loop [brd (if pie?
                  brd1
                  (board/move brd1 p2-first-move))]
       (let [move ((if ((if pie? not identity)
                        (= :white (:turn brd)))
                     player1
                     player2)
                   brd)]
         (if (board/valid-move? move brd)
           (let [new-brd (board/move brd move)]
             (if (board/victory? new-brd)
               (if ((if pie? not identity)
                    (= :white (:turn new-brd)))
                 2
                 1)
               (recur new-brd)))
           (if ((if pie? not identity)
                (= :white (:turn brd)))
             2
             1)))))))

#_(play-game #(do (println "P1:" %) (read))
             #(do (println "P2:" %) (read))
             4)
