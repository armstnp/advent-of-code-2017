(ns advent-of-code-2017.day-22
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day22.txt"))

(defn turn-left
  [dir]
  (get {:north :west
        :west :south
        :south :east
        :east :north}
       dir))

(defn turn-right
  [dir]
  (get {:north :east
        :east :south
        :south :west
        :west :north}
       dir))

(defn infected?
  [c]
  (= c \#))

(defn clean?
  [c]
  (= c \.))

(defn flip-cell
  [c]
  (if (infected? c) \. \#))

(defn cell-turn
  [c dir]
  ((if (infected? c) turn-right turn-left) dir))

(defn move
  [pos dir]
  (mapv + pos (get {:north [-1 0] :east [0 1] :south [1 0] :west [0 -1]} dir)))

(defn prepend-row
  [board]
  (-> board first count (repeat \.) vec (cons board) vec))

(defn append-row
  [board]
  (-> board first count (repeat \.) vec (#(conj board %))))

(defn prepend-column
  [board]
  (mapv #(vec (cons \. %)) board))

(defn append-column
  [board]
  (mapv #(conj % \.) board))

(defn adjust-board
  [{:keys [pos board] :as state}]
  (let [[row col] pos
        board-height (count board)
        board-width (count (first board))]
    (cond
      (= row -1)
      (-> state
        (assoc-in [:pos 0] 0)
        (update :board prepend-row))

      (= col -1)
      (-> state
        (assoc-in [:pos 1] 0)
        (update :board prepend-column))

      (>= row board-height)
      (update state :board append-row)

      (>= col board-width)
      (update state :board append-column)

      :else state)))

(defn burst
  [{:keys [pos dir board] :as state}]
  (let [cell (get-in board pos)
        [row col] pos]
    (as-> state $
      (update $ :dir #(cell-turn cell %))
      (update-in $ [:board row col] flip-cell)
      (update $ :pos #(move % (get $ :dir)))
      (adjust-board $))))

(defn start-state
  [board]
  {:pos [(/ (dec (count board)) 2) (/ (dec (count (first board))) 2)]
   :dir :north
   :board board})

(defn will-infect?
  [{:keys [pos board]}]
  (clean? (get-in board pos)))

(->> input
  str/split-lines
  (mapv vec)
  start-state
  (iterate burst)
  (take 10000)
  (filter will-infect?)
  count)
