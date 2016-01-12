(ns board)

;; (defrecord Board [size board turn unions])

(defn starting-board
  "Pseudo-nodes for sides and two nodes in the middle
   of each side."
  ([]
   (starting-board 12))
  ([size]
   (let [half (int (/ size 2))]
     {:turn :black
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
              cells (filter (complement cell/pseudo-node?)
                            (keys board))]
         (if (empty? cells)
           union
           (let [[nxt & rst] cells
                 dfs (fn dfs [[union visited] child]
                       (if (good-node? child visited)
                         (reduce
                          dfs
                          [(assoc union child nxt)
                           (assoc visited child true)]
                          (if (cell/pseudo-node? child)
                            []
                            (cell/neighbours child brd)))
                         [union visited]))]
             (if (good-node? nxt visited)
               (recur ;; we've got a new union
                (reduce dfs
                        [(assoc union nxt nxt)
                         (assoc visited nxt true)]
                        (cell/neighbours nxt brd))
                rst)
               (recur [union visited] rst))))))))

#_(get-union (starting-board))

(defn get-union-cell
  "Assumes that the cell is occupied by the relevant color."
  [cell union]
  (let [parent (get union cell)]
    (if (= parent cell)
      [parent union]
      (let [[real-parent new-union] (get-union-cell parent union)]
        [real-parent (assoc new-union cell real-parent)]))))

#_(get-union-cell :top (connect [1 1] (starting-board)))

(defn connect
  "Given a new position on a board, return a new union."
  [cell {:keys [board turn] :as brd}]
  (let [[cells union]
        (reduce
         (fn [[cells union] cell]
           (let [[new-cell new-union]
                 (get-union-cell cell union)]
             [(conj cells new-cell) new-union]))
         [[cell] (get-union brd)]
         (filter #(= turn (get board %))
                 (cell/neighbours cell brd)))]
    (reduce #(assoc %1 %2 cell)
            union
            (distinct cells))))

#_(connect [5 1] (starting-board))

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
        union (get-union
               (assoc brd :turn turn))
        [one union] (get-union-cell one union)
        [another union] (get-union-cell another union)]
    [(= one another) union]))

#_(-> (starting-board 4)
      (move [2 2])   ; b
      first
      (move [3 3])   ; w
      first
      (move [2 3])) ; b victory

(defn move
  "Return a new board and whether it was a victorious move."
  [{:keys [turn board] :as brd} cell]
  (let [brd (-> brd
                (assoc-in [:board cell] turn)
                (assoc-in [:unions turn] (connect cell brd))
                (capture cell))
        [result union] (victory? brd)]
    [(-> brd
         (assoc-in [:unions turn] union)
         (assoc :turn (opposite-turn turn)))
     result]))

#_(move (starting-board) [1 2])
