(def input [106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118])
(def str-input "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118")
 
(defn step
  [{:keys [hash-list current-pos skip-size lengths] :as state}]
  (let [list-length (count hash-list)
        circular-list (cycle hash-list)
        [curr-length & rest-lengths] lengths
        reversed-section (reverse (take curr-length (drop current-pos circular-list)))
        initial-replacement (concat (take current-pos hash-list)
                                    reversed-section
                                    (drop (+ current-pos curr-length) hash-list))
        [base-replacement extra-at-end] (split-at list-length initial-replacement)
        cycled-replacement (concat extra-at-end
                                   (drop (count extra-at-end) base-replacement))]
    {:hash-list cycled-replacement
     :current-pos (mod (+ current-pos curr-length skip-size) list-length)
     :skip-size (inc skip-size)
     :lengths rest-lengths}))

(->> {:hash-list (range 256)
      :current-pos 0
      :skip-size 0
      :lengths input}
  (iterate step)
  (drop-while :lengths)
  first
  :hash-list
  (take 2)
  (apply *))

(->> {:hash-list (range 256)
      :current-pos 0
      :skip-size 0
      :lengths (->> str-input
                 (map int)
                 (#(concat % [17 31 73 47 23]))
                 cycle
                 (take (* (+ 5 (count str-input)) 64)))}
  (iterate step)
  (drop-while :lengths)
  first
  :hash-list
  (partition 16)
  (map #(apply bit-xor %))
  (map #(format "%02x" %))
  (apply str))
