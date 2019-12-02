(ns intcode
  (:require [clojure.string :refer [split]]))

(defn lex [f]
  (-> (slurp "./resources/intcode.txt")
      (split #",")
      (->> (map #(Integer/parseInt %)))
      vec))

(defn intcode-op [intcode pos op]
  (let [opr1 (nth intcode (nth intcode (inc pos)))
        opr2 (nth intcode (nth intcode (+ pos 2)))]
    (assoc intcode
      (nth intcode (+ pos 3))
      (op opr1 opr2))))

(defn apply-intcode-op [intcode pos]
  (case (nth intcode pos)
    1 (intcode-op intcode pos +)
    2 (intcode-op intcode pos *)
    nil))

(defn interpret [intcode]
  (loop [state intcode
         pos 0]
    (if-let [new-state (apply-intcode-op state pos)]
      (recur new-state (+ pos 4))
      state)))

(defn run [f]
  (-> (lex f)
      interpret))

(defn parameterize [intcode noun verb]
  (-> (assoc intcode 1 noun)
      (assoc 2 verb)))

(defn find-output [f n]
  ;; Code to solve Day 2 Goal 2
  (let [intcode (lex f)]
    (->> (for [noun (range 0 99)
               verb (range 0 99)]
           (parameterize intcode noun verb))
         (map interpret)
         (filter #(= n (first %)))
         first)))
