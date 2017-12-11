(ns advent-of-code-2017.day-9
  (:require [advent-of-code-2017.core :as core]))

(def input (core/read-input "day9.txt"))

(defn start-group
  [{:keys [level score] :as state}]
  (let [new-level (inc level)]
    (assoc state :level new-level :score (+ score new-level))))

(defn end-group [state] (update state :level dec))

(defn start-garbage [state] (assoc state :parse-state :parsing-garbage))

(defn end-garbage [state] (assoc state :parse-state :parsing-group))

(defn add-garbage [state] (update state :garbage-count inc))

(defn shift-stream [state] (update state :stream next))

(def default-action
  {:parsing-group identity
   :parsing-garbage add-garbage})

(defn get-action
  [{:keys [parse-state stream] :as state}]
  (case [parse-state (first stream)]
    [:parsing-group   \{ ] start-group
    [:parsing-group   \} ] end-group
    [:parsing-group   \< ] start-garbage
    [:parsing-garbage \> ] end-garbage
    [:parsing-garbage \! ] shift-stream
    (default-action parse-state)))

(defn gulp
  [state]
  (let [action (get-action state)]
    (shift-stream (action state))))

(defn solve
  [input]
  (->> {:parse-state :parsing-group
        :score 0
        :garbage-count 0
        :level 0
        :stream input}
    (iterate gulp)
    (drop-while :stream)
    first
    (#(select-keys % [:score :garbage-count]))))

(defn test-score
  [input score]
  (-> input
       solve
       :score
       (= score)
       (assert (str input " does not score " score))))

(defn test-garbage
  [input garbage-count]
  (-> input
      solve
      :garbage-count
      (= garbage-count)
      (assert (str input " does not garbage-count " garbage-count))))

(defn test-all
  [test-fn & cases]
  (dorun (map (partial apply test-fn) cases)))

(test-all test-score
  ["{}" 1]
  ["{{{}}}" 6]
  ["{{},{}}" 5]
  ["{{{},{},{{}}}}" 16]
  ["{<a>,<a>,<a>,<a>}" 1]
  ["{{<ab>},{<ab>},{<ab>},{<ab>}}" 9]
  ["{{<!!>},{<!!>},{<!!>},{<!!>}}" 9]
  ["{{<a!>},{<a!>},{<a!>},{<ab>}}" 3])

(test-all test-garbage
  ["<>" 0]
  ["<random characters>" 17]
  ["<<<<>" 3]
  ["<{!>}>" 2]
  ["<!!>" 0]
  ["<!!!>>" 0]
  ["<{o\"i!a,<{i<a>" 10])

(solve input)
