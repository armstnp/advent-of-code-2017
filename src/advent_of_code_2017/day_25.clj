(ns advent-of-code-2017.day-25)

(defn tape-write
  [{:keys [pointer] :as state} val]
  (assoc-in state [:tape pointer] val))

(defn tape-write-0
  [state]
  (tape-write state 0))

(defn tape-write-1
  [state]
  (tape-write state 1))

(defn pointer-move-left
  [{:keys [pointer] :as state}]
  (if (zero? pointer)
    (update state :tape #(vec (cons 0 %)))
    (update state :pointer dec)))

(defn pointer-move-right
  [{:keys [tape pointer] :as state}]
  (let [new-pointer (inc pointer)
        new-pointed-state (assoc state :pointer new-pointer)]
    (if (= new-pointer (count tape))
      (update new-pointed-state :tape #(conj % 0))
      new-pointed-state)))

(defn set-state-id
  [state-id state]
  (assoc state :state-id state-id))

(defn run-step
  [state write move state-id]
  (->> state
    ((case write 0 tape-write-0 1 tape-write-1))
    ((case move :left pointer-move-left :right pointer-move-right))
    (set-state-id state-id)))

(def state-map
  {:a [[1 :right :b] [0 :left :b]]
   :b [[1 :left :c] [0 :right :e]]
   :c [[1 :right :e] [0 :left :d]]
   :d [[1 :left :a] [1 :left :a]]
   :e [[0 :right :a] [0 :right :f]]
   :f [[1 :right :e] [1 :right :a]]})

(def num-steps 12683008)

(defn next-state
  [{:keys [tape pointer state-id] :as state}]
  (let [curr-val (get tape pointer)
        [write move state-id] (get-in state-map [state-id curr-val])]
    (run-step state write move state-id)))

(def initial-state {:tape [0] :pointer 0 :state-id :a})

(->> initial-state
  (iterate next-state)
  (drop num-steps)
  first
  :tape
  (reduce +))
