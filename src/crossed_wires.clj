(ns crossed-wires
  (:require [clojure.string :refer [split split-lines]]
            [clojure.set :refer [intersection]]))

(defn parse-move [s]
  {:dir (-> (.charAt s 0) str keyword)
   :steps (-> (subs s 1) Integer/parseInt)})

(defn load-moves [s]
  (->> (split-lines s)
       (map #(split % #","))
       (map #(map parse-move %))))

(defn rrange [start end]
  (-> (if (< start end)
        (range (inc start) (inc end))
        (reverse (range end start)))))

(defn move-to [pos move]
  (let [{x :x y :y} pos
        {dir :dir steps :steps} move]
    (case dir
      :R (->> (rrange x (+ x steps)) (map #(do {:x % :y y})))
      :L (->> (rrange x (- x steps)) (map #(do {:x % :y y})))
      :U (->> (rrange y (+ y steps)) (map #(do {:x x :y %})))
      :D (->> (rrange y (- y steps)) (map #(do {:x x :y %}))))))

(defn walk [moves]
  (loop [remaining-moves moves
         path [{:x 0 :y 0}]]
    (if-let [move (first remaining-moves)]
      (recur (rest remaining-moves)
             (concat path (move-to (last path) move)))
      path)))

(defn manhattan [p q]
  (let [{a :x b :y} p
        {c :x d :y} q]
    (+ (Math/abs (- a c))
       (Math/abs (- b d)))))

;; Solution to Problem #1
(defn central-intersection [s]
  (->> (load-moves s)
       (map walk)
       (map set)
       (apply intersection)
       (map #(manhattan {:x 0 :y 0} %))
       (filter pos-int?)
       (apply min)))

(defn steps [route target]
  (->> route
       (take-while #(not= % target))
       count))

(defn total-steps [intersection routes]
  (->> routes
       (map #(steps % intersection))
       (reduce +)))

;; Solution to Problem 2
(defn min-signal-delay [s]
  (let [routes (->> (load-moves s)
                    (map walk))
        intersections (->> (map set routes)
                           (apply intersection)
                           (filter #(not= % {:x 0 :y 0})))]
    (println routes)
    (->> (map #(total-steps % routes) intersections)
         (apply min))))