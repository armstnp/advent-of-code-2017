(ns advent-of-code-2017.day-18-b
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.core :as core]))

(def input (core/read-input "day18.txt"))
(def instruction-lines (->> input str/split-lines (map #(str/split % #" "))))

(defn viable-int?
  [s]
  (re-matches #"-?\d+" s))

(core/defn-split build-arg-eval
  [arg | registers]
  (if (viable-int? arg)
    (core/parse-int arg)
    (get registers arg 0)))

(core/defn-split build-reg-set
  [register | value | registers]
  (assoc registers register value))

(defn inc-instr
  [state]
  (update state :instr-pointer inc))

(defn not-awaiting
  [state]
  (assoc state :awaiting false))

(defn awaiting
  [state]
  (assoc state :awaiting true))

(core/defn-split parse-snd
  [eval-x | {:keys [registers] :as state}]
  (-> state
    (assoc :emitting (eval-x registers))
    inc-instr
    not-awaiting))

(core/defn-split parse-set
  [eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (eval-y registers)))
    inc-instr
    not-awaiting))

(core/defn-split parse-add
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (+ (eval-x registers) (eval-y registers))))
    inc-instr))

(core/defn-split parse-mul
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (* (eval-x registers) (eval-y registers))))
    inc-instr
    not-awaiting))

(core/defn-split parse-mod
  [eval-x eval-y set-x | {:keys [registers] :as state}]
  (-> state
    (update :registers (set-x (rem (eval-x registers) (eval-y registers))))
    inc-instr
    not-awaiting))

(core/defn-split parse-rcv
  [set-x | {:keys [registers in-queue] :as state}]
  (let [[next-queue & rest-queue] in-queue]
    (if (nil? next-queue)
      (awaiting state)
      (-> state
        (update :registers (set-x next-queue))
        (assoc :in-queue (vec rest-queue))
        inc-instr
        not-awaiting))))

(core/defn-split parse-jgz
  [eval-x eval-y | {:keys [registers instr-pointer] :as state}]
  (not-awaiting
    (let [test-val (eval-x registers)
          jump-offset (eval-y registers)]
      (if (> test-val 0)
        (assoc state :instr-pointer (+ instr-pointer jump-offset))
        (inc-instr state)))))

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
      "rcv" (parse-rcv set-x)
      "jgz" (parse-jgz eval-x eval-y))))

(core/defn-split build-instruction-runner
  [instructions | {:keys [instr-pointer] :as state}]
  (if-let [instruction (get instructions instr-pointer)]
    (instruction state)
    (assoc state :complete true)))

(def instruction-runner
  (->> instruction-lines
    (map parse-instruction)
    vec
    build-instruction-runner))

(def initial-state {:registers {} :instr-pointer 0 :in-queue []})
(def initial-programs [{:registers {"p" 0} :instr-pointer 0 :in-queue []}
                       {:registers {"p" 1} :instr-pointer 0 :in-queue []}])

(defn is-halted?
  [{:keys [complete awaiting]}]
  (or complete awaiting))

(defn enqueue-some
  [queue v]
  (if v (conj queue v) queue))

(defn set-synced-state
  [prog queue]
  (assoc prog :emitting nil :in-queue queue))

(defn sync-programs
  [[{emitting-a :emitting in-queue-a :in-queue :as prog-a}
    {emitting-b :emitting in-queue-b :in-queue :as prog-b}]]
  (let [new-a-queue (enqueue-some in-queue-a emitting-b)
        new-b-queue (enqueue-some in-queue-b emitting-a)
        synced-a (set-synced-state prog-a new-a-queue)
        synced-b (set-synced-state prog-b new-b-queue)]
    [synced-a synced-b]))

(core/defn-split build-program-stepper
  [instruction-runner | progs]
  (let [synced-progs (sync-programs progs)
        stepped-progs (map instruction-runner synced-progs)
        all-halted (every? is-halted? stepped-progs)]
    (when-not all-halted stepped-progs)))

(->> initial-programs
  (iterate (build-program-stepper instruction-runner))
  (take-while identity)
  (filter #(get (second %) :emitting))
  count)
