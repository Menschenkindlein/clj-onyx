(ns board
  (:require cell))

;; (defrecord Board [size board turn unions])

;; A turn in the board is like the last turn.
;; When move is being done, it takes control over the board,
;; setting its turn to its color.

(defn starting-board
  "Pseudo-nodes for sides and two nodes in the middle
   of each side."
  ([]
   (starting-board 12))
  ([size]
   (let [half (int (/ size 2))]
     {:turn :white ; like last move
      :size size
      :board {:left :white  ; pseudo-nodes
              :right :white ; for sides
              [1 half] :white       ; and two nodes
              [1 (+ half 1)] :white ; in the middle
              [size half] :white ; on each side
              [size (+ half 1)] :white
              ;; the same for black
              :top :black
              :bottom :black
              [half 1] :black
              [(+ half 1) 1] :black
              [half size] :black
              [(+ half 1) size] :black}})))

#_(starting-board)

(defn valid-move?
  "A cell must fit into the size of the board
   it has to be free, and if it's a center of a square
   all corners of that square must be free."
  [cell {:keys [board] :as the-board}]
  (and (cell/valid-cell? cell the-board)
       (not (get board cell))
       (or (not (cell/square-center? cell))
           (every? #(valid-move? % the-board)
                   (cell/neighbours cell the-board)))))

#_(valid-move? [1 2] {:size 12 :board {[1 1] :white}})
#_(valid-move? [[2 3] [1 2]] {:size 12 :board {[2 1] :white}})

(defn get-union
  "Given a board (with turn), return the corresponding union.
   It's like a getter, but it rebuilds the union if it becomes
   outdated (e.g., when some pieces were captured)"
  [{:keys [unions turn board] :as brd}]
  (or (turn unions)
      (letfn [(good-node? [cell visited]
                (and (= turn (get board cell))
                     (not (get visited cell))))]
       (loop [[union visited] [{} {}]
              cells (keys board)]
         (if (empty? cells)
           union
           (let [[nxt & rst] cells
                 dfs (fn dfs [[union visited count] child]
                       (if (good-node? child visited)
                         (reduce
                          dfs
                          [(assoc union child nxt)
                           (assoc visited child true)
                           (+ count 1)]
                          (cell/neighbours child brd))
                         [union visited count]))]
             (if (good-node? nxt visited)
               ;; we've got a new union
               (let [[union visited count]
                     (reduce dfs
                             [union
                              (assoc visited nxt true)
                              1]
                             (cell/neighbours nxt brd))]
                (recur
                 [(assoc union nxt [nxt count]) visited]
                 rst))
               (recur [union visited] rst))))))))

#_(get-union (starting-board))
#_(get-union {:turn :black,
              :size 12,
              :board {[7 1] :black,
                      [7 12] :black,
                      :bottom :black,
                      :top :black,
                      [6 1] :black,
                      [6 12] :black,
                      [12 12] :black}})

(defn get-union-cell
  "Returns a root cell together with the count of its descendants.
   Assumes that the cell is occupied by the relevant color."
  [cell union]
  (let [parent (get union cell)]
    (if (nil? parent) ; [cell count] never coincides with a real cell
      cell
      (get-union-cell parent union))))

#_(get-union-cell :top (connect [1 1] (starting-board)))

(defn connect
  "Given a new position on a board, return a new union."
  [cell {:keys [board turn] :as brd}]
  (let [union (get-union brd)
        cells (->> (cell/neighbours cell brd)
                   (filter #(= turn (get board %)))
                   (map #(get-union-cell % union))
                   distinct
                   (sort #(> (second %1) (second %2))))
        count (reduce + (map second cells))
        cells (map first cells)]
    (if (empty? cells)
      (assoc union cell [cell 1])
      (let [[best-cell & cells] cells]
        (reduce #(assoc %1 %2 best-cell)
                (assoc union best-cell [best-cell (+ count 1)])
                (conj cells cell))))))

#_(connect [5 1] (starting-board))
#_(connect [5 5] (starting-board))

(defn opposite-turn
  [turn]
  (case turn
    :white :black
    :black :white))

(defn relevant-square?
  "Opposite corner is occupied by the same color,
   center isn't occupied, other corners are occupied
   by the enemy."
  [{:keys [opposite center corners]} {:keys [board turn]}]
  (and (nil? (get board center))
       (= turn (get board opposite))
       (every? #(= (opposite-turn turn)
                   (get board %))
               corners)))

#_(let [brd {:turn :white
             :size 12
             :board {[2 1] :black,
                     [3 2] :black,
                     [2 2] :white,
                     [3 1] :white}}]
    (relevant-square? (first (cell/squares [3 1] brd)) brd))

(defn capture
  "If it is possible to capture anything, remove them from
   the board, and set the opposite color's union to `nil`
   as irrelevant."
  [{:keys [turn] :as brd} cell]
  (let [squares (cell/squares cell brd)]
    (if (empty? squares)
      brd
      (-> brd
          (update :unions dissoc (opposite-turn turn))
          (update :board
                  (partial reduce
                           #(dissoc %1 %2))
                  (mapcat #(when (relevant-square? % brd)
                             (:corners %))
                         squares))))))

#_(let [brd {:turn :white
             :size 12
             :unions {:black {[2 1] [2 1],
                              [3 2] [3 2]}}
             :board {[2 1] :black,
                     [3 2] :black,
                     [2 2] :white,
                     [3 1] :white}}]
    (capture brd [3 1]))

(defn victory?
  "Check if the relevant opposite sides of the board are connected.
   Also, return a union that is updated while checking."
  [{:keys [turn] :as brd}]
  (let [[one another] (case turn
                        :white [:left :right]
                        :black [:top :bottom])
        union (get-union brd)]
    (= (get-union-cell one union)
       (get-union-cell another union))))

#_ (victory? (-> (starting-board 4)
                 (move [2 2])                    ; b
                 (move [3 3])                    ; w
                 (move [2 3])))                  ; b victory

(defn move
  "Return a new board and whether it was a victorious move."
  [{:keys [turn board] :as brd} cell]
  (let [turn (opposite-turn turn)
        brd (assoc brd :turn turn)]
    (-> brd
        (assoc-in [:board cell] turn)
        (assoc-in [:unions turn] (connect cell brd))
        (capture cell))))

#_(move (starting-board) [1 2])
