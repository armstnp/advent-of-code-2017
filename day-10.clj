(def input [106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118])
(def str-input "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118")

(def ring-length 256)
(def starting-ring (range ring-length))

(def hash-lengths-suffix [17 31 73 47 23])

(defn twist
  [{:keys [ring pos skip lengths] :as state}]
  (let [[segment-length & rest-lengths] lengths
        
        ;; `cycle` guarantees we'll wrap around to get the full segment length
        reversed-segment (->> ring
                           cycle
                           (drop pos)
                           (take segment-length)
                           reverse)
                         
        ;; Splat the replacement in, not caring if it goes over the end of the ring's normal size
        initial-replacement (concat (take pos ring)
                                    reversed-segment
                                    (drop (+ pos segment-length) ring))
                                  
        ;; Split off the part that's sticking off the end, if any
        [base-replacement extra-at-end] (split-at ring-length initial-replacement)
        
        ;; Stick the part that was hanging off the end back at the beginning
        pasted-replacement (->> base-replacement
                             (drop (count extra-at-end))
                             (concat extra-at-end))]

    {:ring pasted-replacement
     :pos (mod (+ pos segment-length skip) ring-length)
     :skip (inc skip)
     :lengths rest-lengths}))

(defn knot
  [start-state]
  (->> start-state
     (iterate twist)
     (drop-while :lengths)
     first
     :ring))

(defn make-starting-state
  [lengths]
  {:ring starting-ring
   :pos 0
   :skip 0
   :lengths lengths})

(defn str->lengths
  [s]
  (->> s
    (map int)
    (#(concat % hash-lengths-suffix))
    cycle
    (take (* 64 (+ (count hash-lengths-suffix) (count s))))))

(defn knot-lengths
  [lengths]
  (knot (make-starting-state lengths)))

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
