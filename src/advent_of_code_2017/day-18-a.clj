(ns advent-of-code-2017.day-18
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day18.txt"))
(def instruction-lines (->> input str/split-lines (map #(str/split % #" "))))

(defn viable-int?
  [s]
  (re-matches #"-?\d+" s))

(core/defn-split build-arg-eval
  [X | registers]
  (if (viable-int? X)
    (core/parse-int X)
    (get registers X 0)))

(core/defn-split build-reg-set
  [X | v | registers]
  (assoc registers X v))

(defn inc-instr
  [state]
  (update state :instr-pointer inc))

(core/defn-split parse-snd
  [eval-x | {:keys [registers] :as state}]
  (-> state
    (assoc :sound-freq (eval-x registers))
    inc-instr))

(core/defn-split parse-set
  [eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (eval-y registers)))
    inc-instr))

(core/defn-split parse-add
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (+ (eval-x registers) (eval-y registers))))
    inc-instr))

(core/defn-split parse-mul
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (* (eval-x registers) (eval-y registers))))
    inc-instr))

(core/defn-split parse-mod
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (rem (eval-x registers) (eval-y registers))))
    inc-instr))

(core/defn-split parse-rcv
  [eval-x | {:keys [registers sound-freq] :as state}]
  (inc-instr
    (if (not= 0 (eval-x registers))
      (assoc state :recovered sound-freq)
      state)))

(core/defn-split parse-jgz
  [eval-x eval-y | {:keys [registers instr-pointer] :as state}]
  (let [test-val (eval-x registers)
        jump-offset (eval-y registers)]
    (if (> test-val 0)
      (assoc state :instr-pointer (+ instr-pointer jump-offset))
      (inc-instr state))))

(defn parse-instruction
  [[selector & args]]
  (let [eval-x (build-arg-eval (first args))
        eval-y (when (> (count args) 1) (build-arg-eval (second args)))
        set-x (build-reg-set (first args))]
    (case selector
      "snd" (parse-snd eval-x)
      "set" (parse-set eval-y set-x)
      "add" (parse-add eval-x eval-y set-x)
      "mul" (parse-mul eval-x eval-y set-x)
      "mod" (parse-mod eval-x eval-y set-x)
      "rcv" (parse-rcv eval-x)
      "jgz" (parse-jgz eval-x eval-y))))

(core/defn-split build-instruction-runner
  [instructions | {:keys [instr-pointer] :as state}]
  (let [instruction (get instructions instr-pointer)]
    (instruction state)))

(def instruction-runner
  (->> instruction-lines
    (map parse-instruction)
    vec
    build-instruction-runner))

(def initial-state {:registers {} :instr-pointer 0})

(->> initial-state
  (iterate instruction-runner)
  (filter :recovered)
  first
  :recovered)
