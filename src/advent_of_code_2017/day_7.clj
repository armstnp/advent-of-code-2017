(ns advent-of-code-2017.day-7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day7.txt"))

(def supporting-program-re #"^([a-z]+) \((\d+)\) -> ([a-z, ]+)$")

(def standalone-program-re #"^([a-z]+) \((\d+)\)$")

(defn parse-supporting-program-line
  [line]
  (if-let [matches (re-matches supporting-program-re line)]
    (let [[_ tower weight subtowers-str] matches
          subtowers (str/split subtowers-str #", ")]
      [tower {:weight (core/parse-int weight) :subtowers subtowers}])))

(defn parse-standalone-program-line
  [line]
  (let [matches (re-matches standalone-program-re line)
        [_ tower weight] matches]
    [tower {:weight (core/parse-int weight) :subtowers []}]))

(defn parse-line
  [line]
  (if-let [supporting-program (parse-supporting-program-line line)]
    supporting-program
    (parse-standalone-program-line line)))

(defn all-programs
  [program-defs]
  (->> program-defs
    (map first)
    (into #{})))

(defn all-subtowers
  [program-defs]
  (->> program-defs
    (map #(get-in % [1 :subtowers]))
    (reduce into #{})))

(defn base-program
  [program-defs]
  (first (set/difference (all-programs program-defs)
                         (all-subtowers program-defs))))

(defn program-tree
  [program-defs]
  (into {} program-defs))

(defn subtowers-of
  [tree program]
  (get-in tree [program :subtowers]))

(defn weight-of
  [tree program]
  (get-in tree [program :weight]))

(defn aggregate-weight
  [tree tower]
  (let [program-weight (weight-of tree tower)
        subtowers (subtowers-of tree tower)
        aggregated-subtrees (reduce aggregate-weight tree subtowers)
        new-subtree-weights (map #(weight-of aggregated-subtrees %) subtowers)
        new-weight (reduce + program-weight new-subtree-weights)]
    (assoc-in aggregated-subtrees [tower :weight] new-weight)))

(defn unbalanced
  [subtowers subtower-weights]
  (let [freqs (frequencies subtower-weights)
        diff-weight (core/reverse-lookup freqs 1)
        subtowers-to-weights (map vector subtowers subtower-weights)]
    (core/reverse-lookup subtowers-to-weights diff-weight)))

(defn seek-misweighted
  ([tree base-program]
   (let [subtowers (subtowers-of tree base-program)
         subtower-weights (map #(weight-of tree %) subtowers)
         unbalanced-program (unbalanced subtowers subtower-weights)
         unbalanced-weight (weight-of tree unbalanced-program)
         other-weights (first (filter #(not= unbalanced-weight %) subtower-weights))
         expected-diff (- other-weights unbalanced-weight)]
     (seek-misweighted tree unbalanced-program expected-diff)))

  ([tree program diff]
   (let [subtowers (subtowers-of tree program)]
     (if (= 2 (count subtowers))
       (let [[program-a program-b] subtowers
             [weight-a weight-b] (map #(weight-of tree %) subtowers)]
         (cond
           (= weight-a weight-b) [program diff]
           (= (+ weight-a diff) weight-b) (seek-misweighted tree program-a diff)
           :else (seek-misweighted tree program-b diff)))
       (let [subtower-weights (map #(weight-of tree %) subtowers)
             unbalanced-program (unbalanced subtowers subtower-weights)]
         (if-not unbalanced-program
           [program diff]
           (seek-misweighted tree unbalanced-program diff)))))))

(defn rebalanced-weight
  [input]
  (let [parsed-defs (map parse-line (str/split-lines input))
        base (base-program parsed-defs)
        regular-tree (program-tree parsed-defs)
        aggregate-tree (aggregate-weight regular-tree base)
        [misweighted-program diff-weight] (seek-misweighted aggregate-tree base)]
    (+ (weight-of regular-tree misweighted-program) diff-weight)))
