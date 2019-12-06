(ns orbit-map
  (:require [clojure.string :refer [split-lines split]]))

(defn treefy [com orbits]
  (if (contains? orbits com)
    (conj (map #(treefy % orbits) (get orbits com)) com)
    (seq [com])))

(defn orbit-tree-seq [input]
  (->> (split-lines input)
       (map #(split % #"\)"))
       (map #(map keyword %))
       (group-by first)
       (reduce-kv #(assoc %1 %2 (map last %3)) {})
       (treefy :COM)
       (tree-seq next rest)))

(defn count-orbits [orbit-tree]
  (->> orbit-tree
       (map #(-> (flatten %)
                 count
                 dec))
       (reduce +)))

(defn tree-contains? [tree value]
  (-> (flatten tree)
      set
      (contains? value)))

(defn walk [orbits]
  (->> (filter #(and (tree-contains? % :SAN)
                     (tree-contains? % :YOU))
               orbits)
       (sort-by #(count (flatten %)))
       first
       (tree-seq next rest)
       rest
       (filter #(not= (tree-contains? % :SAN)
                      (tree-contains? % :YOU)))
       (remove #(= 1 (count %)))
       count))