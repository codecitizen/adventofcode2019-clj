(ns password-cracker)

(defn comb
  ([digit base] (comb digit 0 base))
  ([digit value base] (if (= base 1)
                        (->> (range digit 10)
                             (map #(+ value %)))
                        (-> (for [next (range digit 10)]
                              (comb next (+ value (* base next)) (/ base 10)))
                            flatten))))

;; Solution to Problem #1
(defn count-pw-combs []
  (->> (comb 1 0 100000)
       (filter #(<= 128392 % 643281))
       (filter #(not (apply distinct? (str %))))
       count))

(defn has-pair [n]
  (->> (re-seq #"(.)\1*" (str n))
       (map first)
       (filter #(= 2 (.length %)))
       not-empty))

;; Solution to Problem #2
(defn count-pw-combs-2 []
  (->> (comb 1 0 100000)
       (filter #(<= 128392 % 643281))
       (filter has-pair)
       count))
