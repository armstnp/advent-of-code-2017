(def input [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])

(defn find-index
  [v n]
  (->> v
    (map vector (range))
    (drop-while #(not= n (second %)))
    first
    first))

(defn make-distribution
  [v n curr]
  (if (>= curr (count v))
    (recur v n 0)
    (if (= n 0)
      v
      (recur (update v curr inc) (dec n) (inc curr)))))

(defn distribute-next
  [v]
  (let [most-blocks (apply max v)
        bank-to-distribute (find-index v most-blocks)
        drained-bank (assoc v bank-to-distribute 0)]
    (make-distribution drained-bank most-blocks (inc bank-to-distribute))))

(defn distribute-and-test
  [seen-distributions banks]
  (if (contains? seen-distributions banks)
    (count seen-distributions)
    (recur (conj seen-distributions banks) (distribute-next banks))))

(defn find-matching-state
 [seen-distributions banks]
 (if (contains? seen-distributions banks)
   banks
   (recur (conj seen-distributions banks) (distribute-next banks))))
