(ns advent-of-code-2017.day-23
  (:require [clojure.string :as str]
            [advent-of-code-2017.core :as core]
            [is-prime.core :as ip :refer [is-prime]]))

(def input (core/read-input "day23.txt"))
(def instruction-lines (->> input str/split-lines (map #(str/split % #" "))))

(defn viable-int?
  [s]
  (re-matches #"-?\d+" s))

(defn build-arg-eval
  [arg]
  (if (viable-int? arg)
    (let [int-val (core/parse-int arg)]
      (fn [_] int-val))
    (fn [registers] (get registers arg 0))))

(core/defn-split build-reg-set
  [register | value | registers]
  (assoc registers register value))

(defn inc-instr
  [state]
  (update state :instr-pointer inc))

(core/defn-split parse-set
  [eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (eval-y registers)))
    inc-instr))

(core/defn-split parse-sub
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (- (eval-x registers) (eval-y registers))))
    inc-instr))

(core/defn-split parse-mul
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (* (eval-x registers) (eval-y registers))))
    inc-instr))

(core/defn-split parse-jnz
  [eval-x eval-y | {:keys [registers instr-pointer] :as state}]
  (let [test-val (eval-x registers)
        jump-offset (eval-y registers)]
    (if (not (zero? test-val))
      (assoc state :instr-pointer (+ instr-pointer jump-offset))
      (inc-instr state))))

(defn parse-instruction
  [[selector & args]]
  (let [eval-x (build-arg-eval (first args))
        eval-y (when (> (count args) 1) (build-arg-eval (second args)))
        set-x (build-reg-set (first args))]
    (case selector
      "set" (parse-set eval-y set-x)
      "sub" (parse-sub eval-x eval-y set-x)
      "mul" (parse-mul eval-x eval-y set-x)
      "jnz" (parse-jnz eval-x eval-y))))

(core/defn-split build-instruction-runner
  [instructions | {:keys [instr-pointer] :as state}]
  (if-let [instruction (get instructions instr-pointer)]
    (instruction state)))

(def instruction-runner
  (->> instruction-lines
    (map parse-instruction)
    vec
    build-instruction-runner))

(core/defn-split is-mul?
  [instruction-lines | {:keys [instr-pointer]}]
  (let [[selector & _] (get instruction-lines instr-pointer)]
    (= "mul" selector)))

(def initial-state {:registers {} :instr-pointer 0})

(->> initial-state
  (iterate instruction-runner)
  (take-while identity)
  (filter (is-mul? (vec instruction-lines)))
  count)

(->> (range 108100 125101 17)
  (filter (complement ip/is-prime))
  count)

(comment
  In summary, the assembly 'decompiles' to pseudocode similar to the following:

    total = 0
    curr = 108100 to 125100 by 17
      flag = 1
      d = 2 to curr
        e = 2 to curr
          flag = 0 if d*e = curr
      total++ if flag = 0

  This tallies all non-primes at intervals of 17 between 108100 and 125100
  This is more easily traced by seeing each register is used in a different role:
    a = 'debug flag' - when set, the code does a trivial non-prime test for the number 81
    b = 'current tested number' - this is the number being tested for primality
    c = 'end number' - this is the maximum number to be tested for primality
    d = 'multiplicand 1' - this is the first multiplicand used for factor testing
    e = 'multiplicand 2' - this is the second multiplicand used for factor testing
    f = 'prime flag' - this flag is 1 until the number is found to be non-prime, then 0
    g = 'test accumulator' - values to be tested in loops or if statements are computed here
    h = 'non-prime counter' - counts the number of non-primes seen)
