(ns advent-of-code-2017.day-13
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day13.txt"))

(defn parse-line
  [line]
  (->> line
    (re-seq #"\d+")
    (map core/parse-int)))

(def scanners
  (->> input
    str/split-lines
    (map parse-line)))

(defn scanner-catches?
  [[t-pico scan-range]]
  (zero? (mod t-pico (* 2 (dec scan-range)))))

(defn severity
  [[depth scan-range]]
  (* depth scan-range))

(->> scanners
  (filter scanner-catches?)
  (map severity)
  (reduce + 0))

;; A delay is effectively equivalent to pushing each layer deeper (especially
;; since we're ignoring severity entirely for part B
(defn add-delay
  [t-pico [depth scan-range]]
  [(+ depth t-pico) scan-range])

(core/defn-split caught-with-delay?
  [scanners | t-offset]
  (->> scanners
    (map #(add-delay t-offset %))
    (some scanner-catches?)))

(first (drop-while (caught-with-delay? scanners) (range)))
