(def input 361527)
 
(defn grid-val
	[grid [x y]]
	(get-in grid [x y]))

(defn numeric-grid-val
	[grid pos]
	(let [cell-val (grid-val grid pos)]
	  (if cell-val cell-val 0)))

(defn north [[x y]] [x (inc y)])
(defn south [[x y]] [x (dec y)])
(defn east [[x y]] [(inc x) y])
(defn west [[x y]] [(dec x) y])
(defn ccw [dir]
	(case dir
	  :north :west
	  :west :south
	  :south :east
	  :east :north))
	 
(defn move [pos dir]
	(case dir
	  :north (north pos)
	  :south (south pos)
	  :east (east pos)
	  :west (west pos)))

(defn try-turn
	[grid pos dir]
	(let [test-dir (ccw dir)
	      test-pos (move pos test-dir)
	      test-cell (grid-val grid test-pos)]
	  (if-not test-cell test-dir dir)))
	 
(defn surrounding-cell-sum
	[grid [x y]]
	(reduce +
		(for [dx [-1 0 1] dy [-1 0 1]]
		  (numeric-grid-val grid [(+ x dx) (+ y dy)]))))

(def starting-state [{0 {0 1}} [0 0] :east])

(defn step [grid pos dir]
	(let [new-pos (move pos dir)
	      new-dir (try-turn grid new-pos dir)
	      new-val (surrounding-cell-sum grid new-pos)
	      new-grid (assoc-in grid new-pos new-val)]
	  [new-grid new-pos new-dir]))

(defn step-until [grid pos dir max-val]
	(let [curr-val (grid-val grid pos)]
	  (if (> curr-val max-val)
	      curr-val
	      (let [[new-grid new-pos new-dir] (step grid pos dir)]
	         (recur new-grid new-pos new-dir max-val)))))
