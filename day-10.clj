(def input [106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118])
(def str-input "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118")

(def ring-length 256)
(def starting-state
  {:ring (range ring-length)
   :pos 0
   :skip 0})

(def hash-lengths-suffix [17 31 73 47 23])

(defn rotate-left
  [n coll]
  (let [split-point (mod n (count coll))
        [new-start new-end] (split-at split-point coll)]
    (concat new-end new-start)))

(defn rotate-right
  [n coll]
  (rotate-left (- (count coll) (mod n (count coll))) coll))

(defn reverse-first
  [n coll]
  (let [[to-reverse to-preserve] (split-at n coll)]
    (concat (reverse to-reverse) to-preserve)))

(defn twist
  [{:keys [ring pos skip]} twist-length]
  {:ring (->> ring
           (rotate-left pos)
           (reverse-first twist-length)
           (rotate-right pos))
   :pos (mod (+ pos twist-length skip) (count ring))
   :skip (inc skip)})

(defn knot
  [start-state lengths]
  (get (reduce twist start-state lengths) :ring))

(defn str->lengths
  [s]
  (->> s
    (map int)
    (#(concat % hash-lengths-suffix))
    cycle
    (take (* 64 (+ (count hash-lengths-suffix) (count s))))))

(defn knot-lengths
  [lengths]
  (knot starting-state lengths))

(defn knot-string
  [s]
  (knot-lengths (str->lengths s)))

(->> input
  knot-lengths
  (take 2)
  (apply *))

(->> str-input
  knot-string
  (partition 16)
  (map #(apply bit-xor %))
  (map #(format "%02x" %))
  (apply str))
