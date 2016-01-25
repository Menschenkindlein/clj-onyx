(ns cell)

(defn pseudo-node?
  [x]
  (keyword? x))

(defn square-center?
  [x]
  (and (vector? x)
       (vector? (first x))))

#_(square-center? :bottom)

(defn valid-cell?
  "Is it a real cell:
   - does it fit the board?
   - is it a valid square center?"
  [cell {:keys [size]}]
  (and (every? #(<= 1 % size) (flatten cell))
       (if (square-center? cell)
         (let [[[x1 x2] [y1 y2]] cell]
           (and (= 1 (- x2 x1))
                (= 1 (- y2 y1))
                (not (= (odd? x1) (odd? y1)))))
         true)))

#_(valid-cell? [1 0] {:size 12})
#_(valid-cell? [[1 0] [1 2]] {:size 12})
#_(valid-cell? [[1 2] [1 2]] {:size 12})
#_(valid-cell? [[1 2] [2 3]] {:size 12})

(defn neighbours
  "Return the neighbours of the given cell
   in the context of the current board
   (size and turn)"
  [cell {:keys [size turn] :as board}]
  (if (pseudo-node? cell)
    (let [line (range 1 (+ size 1))]
      (case cell
        :left (map #(vector 1 %) line)
        :right (map #(vector size %) line)
        :top (map #(vector % 1) line)
        :bottom (map #(vector % size) line)))
    (let [[x y] cell]
      (if (square-center? cell)
        ;; the order is important for `gui.clj`
        (concat (map vector x y)
                (map vector x (reverse y)))
        (concat
         (cond ; on the border we have pseudo-nodes for connectivity
           (and (= x 1)    (= turn :white)) [:left]
           (and (= x size) (= turn :white)) [:right]
           (and (= y 1)    (= turn :black)) [:top]
           (and (= y size) (= turn :black)) [:bottom])
         (filter #(valid-cell? % board)
                 (let [xy [x y]
                       tupler (comp vec sort vector)
                       direction (if (= (even? x) (even? y))
                                   [1 -1]
                                   [1 1])
                       diagonal (map (partial * (if (even? y) 1 -1))
                                     [(first direction)
                                      (* (second direction) -1)])]
                   [(mapv tupler xy (map + xy direction))
                    (mapv tupler xy (map - xy direction))
                    (mapv + xy diagonal)
                    [x (+ y 1)]
                    [x (- y 1)]
                    [(+ x 1) y]
                    [(- x 1) y]])))))))

#_(neighbours :bottom {:size 12})
#_(neighbours [[2 3] [1 2]] {:size 12})
#_(neighbours [2 2] {:size 12})
#_(neighbours [1 1] {:size 12 :turn :white})
#_(neighbours [2 1] {:size 12 :turn :black})

(defn squares
  "Return the neighbouring squares as maps
   with keys `[opposite, center, corners]`.
   It's a helper function for capture detection."
  [[x y] {:keys [size] :as board}]
  (let [xy [x y]
        tupler (comp vec sort vector) ; order is important for gui
        direction (if (= (even? x) (even? y))
                    [1 -1]
                    [1 1])]
    (filter #(valid-cell? (:opposite %) board)
            (map #(let [opposite (mapv % xy direction)]
                    {:center (mapv tupler xy opposite)
                     :opposite opposite
                     :corners [[(first xy) (second opposite)]
                               [(first opposite) (second xy)]]})
                 [+ -]))))

#_(squares [2 2] {:size 12})
#_(squares [2 1] {:size 12})

(defn all-cells [{:keys [size] :as brd}]
  (filter
    #(valid-cell? % brd)
    (concat
     (for [x (range 1 (+ size 1))
           y (range 1 (+ size 1))]
       [x y])
     (for [x (range 1 size)
           y (range 1 size)]
       [[x (+ x 1)] [y (+ y 1)]]))))
