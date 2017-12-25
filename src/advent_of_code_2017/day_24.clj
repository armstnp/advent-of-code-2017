(ns advent-of-code-2017.day-24
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day24.txt"))

(defn parse-line
  [s]
  (map core/parse-int (str/split s #"/")))

(def components
  (->> input
    str/split-lines
    (map parse-line)
    (mapcat #(list % (vec (reverse %))))
    (group-by first)
    (map (fn [entry] (update entry 1 #(set (mapv second %)))))
    (into {})))

(defn remove-component
  [components port-a port-b]
  (-> components
    (update port-a #(disj % port-b))
    (update port-b #(disj % port-a))))

(defn remove-components
  [components port-a port-bs]
  (reduce #(remove-component %1 port-a %2) components port-bs))

(defn build-bridge
  [components curr-port curr-total]
  (let [next-comps (get components curr-port)
        bridge-strengths (map #(build-bridge (remove-component components curr-port %)
                                             %
                                             (+ curr-total curr-port %))
                              next-comps)]
    (reduce max curr-total bridge-strengths)))

(build-bridge components 0 0)

(defn longest-strongest-bridge
  [bridge-a bridge-b]
  (let [a-length (count bridge-a)
        b-length (count bridge-b)
        a-strength (reduce + bridge-a)
        b-strength (reduce + bridge-b)]
    (cond
      (> a-length b-length) bridge-a
      (< a-length b-length) bridge-b
      (> a-strength b-strength) bridge-a
      :else bridge-b)))

(defn build-long-bridge
  [components [curr-port & rest :as curr-bridge]]
  (let [next-comps (get components curr-port)
        bridges (map #(build-long-bridge (remove-component components curr-port %)
                                         (cons % (cons curr-port curr-bridge)))
                     next-comps)]
    (reduce longest-strongest-bridge curr-bridge bridges)))

(reduce + (build-long-bridge components '(0)))
