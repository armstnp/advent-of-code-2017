(ns advent-of-code-2017.day-16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day16.txt"))

(core/defn-split spin
  [x | dancers]
  (vec (core/rotate-right x dancers)))

(core/defn-split exchange
  [x y | dancers]
  (let [old-x (get dancers x)
        old-y (get dancers y)]
    (assoc dancers x old-y y old-x)))

(core/defn-split partner-swap
  [x y | dancer]
  (condp = dancer
    x y
    y x
    dancer))

(core/defn-split partner
  [x y | dancers]
  (mapv (partner-swap x y) dancers))

(def move-regex #"[sxp][^,]+")

(defn parse-spin
  [params]
  (->> params core/parse-int spin))

(defn parse-exchange
  [params]
  (->> params (re-seq #"\d+") (map core/parse-int) (apply exchange)))

(defn parse-partner
  [params]
  (partner (first params) (nth params 2)))

(defn parse-instruction
  [[selector & params]]
  (case selector
    \s (parse-spin (apply str params))
    \x (parse-exchange (apply str params))
    \p (parse-partner params)))

(def init-dancers (vec "abcdefghijklmnop"))

(def instruction-strings (re-seq move-regex input))

(->> instruction-strings
  (map parse-instruction)
  (reduce (fn [dancers f] (f dancers)) init-dancers))



#_(->> instruction-strings
    (map parse-instruction!)
    cycle
    (take (* 1000000000 (count instruction-strings)))
    (reduce (fn [dancers f] (f dancers)) (transient init-dancers)))


;;  1  0  0  0
;;  0  0  1  0
;;  0  1  0  0
;;  0  0  0  1
