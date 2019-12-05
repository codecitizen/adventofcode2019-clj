(ns intcode
  (:require [clojure.string :refer [split]]))

(defn pop-input! [input-list]
  (let [f (first @input-list)]
    (swap! input-list rest)
    f))

(defn lex [f]
  (-> (slurp f)
      (split #",")
      (->> (map #(Integer/parseInt %)))
      vec))

(defn eval-param [intcode pos param-pos modes]
  (let [param (nth intcode (+ pos param-pos))]
    (case (nth modes (dec param-pos) :position)
      :position (nth intcode param)
      :immediate param)))

(defn intcode-op [intcode pos op modes]
  (let [opr1 (eval-param intcode pos 1 modes)
        opr2 (eval-param intcode pos 2 modes)]
    (assoc intcode
      (nth intcode (+ pos 3))
      (op opr1 opr2))))

(defn take-input! [intcode pos input]
  (assoc intcode
    (nth intcode (+ pos 1))
    (pop-input! input)))

(defn output! [intcode pos modes]
  (println (eval-param intcode pos 1 modes))
  intcode)

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn param-mode [n]
  (case n
    0 :position
    1 :immediate))

(defn operation [n]
  {:op (mod n 100)
   :param-modes (->> (/ n 100)
                     int
                     digits
                     (map param-mode)
                     reverse)})

(defn jump-if [intcode pos pred param-mode]
  (let [param (eval-param intcode pos 1 param-mode)
        new-pos (eval-param intcode pos 2 param-mode)]
    (if (pred param)
      {:new-state intcode
       :pos new-pos}
      {:new-state intcode
       :pos (+ pos 3)})))


(defn apply-intcode-op [intcode pos input]
  (let [{op :op param-modes :param-modes}
        (operation (nth intcode pos))]
    (case op
      1 {:new-state (intcode-op intcode pos + param-modes)
         :pos (+ pos 4)}
      2 {:new-state (intcode-op intcode pos * param-modes)
         :pos (+ pos 4)}
      3 {:new-state (take-input! intcode pos input)
         :pos (+ pos 2)}
      4 {:new-state (output! intcode pos param-modes)
         :pos (+ pos 2)}
      5 (jump-if intcode pos #(not (zero? %)) param-modes)
      6 (jump-if intcode pos zero? param-modes)
      7 {:new-state (intcode-op intcode pos #(if (< %1 %2) 1 0) param-modes)
         :pos (+ pos 4)}
      8 {:new-state (intcode-op intcode pos #(if (= %1 %2) 1 0) param-modes)
         :pos (+ pos 4)}
      nil)))

(defn interpret [intcode input]
  (loop [state intcode
         pos 0]
    (if-let [out (apply-intcode-op state pos input)]
      (let [{new-state :new-state new-pos :pos} out]
        (recur new-state new-pos))
      state)))

(defn run [f input]
  (-> (lex f)
      (interpret input)))

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
