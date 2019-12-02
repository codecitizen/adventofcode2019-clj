(ns santa
  (:require [clojure.string :refer [split-lines]]))

(defn fuel [mass]
  (-> (int (Math/floor (/ mass 3)))
      (- 2)))

(defn carry-fuel [mass]
  (loop [f (fuel mass)
         total 0]
    (if (neg-int? f)
      total
      (recur (fuel f) (+ f total)))))

(defn masses-sum [f]
  (->> (slurp f)
       split-lines
       (map #(Integer/parseInt %))
       (map carry-fuel)
       (reduce +)))