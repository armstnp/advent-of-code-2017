(ns advent-of-code-2017.day-8
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day8.txt"))

(def instruction-re #"([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (\p{Punct}+) (-?\d+)")

(def test-ops
  {">" >
   "<" <
   ">=" >=
   "<=" <=
   "==" =
   "!=" not=})

(def mod-ops {"inc" + "dec" -})

(defn parse-instruction
  [s]
  (let [[_ mod-reg mod-op mod-diff test-reg test-op test-val] (re-matches instruction-re s)
        mod-diff-int (core/parse-int mod-diff)
        mod-op-fn (get mod-ops mod-op)
        test-val-int (core/parse-int test-val)
        test-op-fn (get test-ops test-op)]
    {:mod-reg mod-reg
     :mod-fn #(mod-op-fn % mod-diff-int)
     :test-reg test-reg
     :test-fn #(test-op-fn % test-val-int)}))

(core/defn-split construct-test
  [{:keys [test-reg test-fn]} | context]
  (test-fn (get context test-reg 0)))

(core/defn-split construct-op
  [{:keys [mod-reg mod-fn]} | context]
  (->> (get context mod-reg 0)
    mod-fn
    (assoc context mod-reg)))

(core/defn-split construct-instruction
  [instruction | context]
  (let [test-fn (construct-test instruction)
        op-fn (construct-op instruction)]
    (if (test-fn context)
      (op-fn context)
      context)))

(defn instructions->fns
  [input]
  (->> input
    str/split-lines
    (map parse-instruction)
    (map construct-instruction)))

(defn apply-instruction
  [context instruction]
  (instruction context))

(->> input
     instructions->fns
     (reduce apply-instruction {})
     vals
     (apply max))

(->> input
     instructions->fns
     (reductions apply-instruction {})
     (map vals)
     (reduce into #{})
     (apply max))
