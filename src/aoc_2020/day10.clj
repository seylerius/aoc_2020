(ns aoc-2020.day10
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def input (line-seq (io/reader (io/resource "input10"))))

(defn jolt-distance [d j]
  (- j d))

(defn device-joltage [jolts]
  (+ 3 (last jolts)))

(defn joltify [input]
  (let [jolts (sort (map edn/read-string input))]
    (concat jolts [(device-joltage jolts)])))

(defn valid-joltage? [d j]
  (let [dist (jolt-distance d j)]
    (and (pos? dist) (>= 3 dist))))

(defn joltage-in-range [d jolts]
  (sort (filter (partial valid-joltage? d) jolts)))

(defn joltage-differences [jolts]
  (map (fn [[a b]] (math/abs (- a b))) (partition 2 1 (cons 0 jolts))))

(defn pair-diffs [jolts]
  (map vector jolts (joltage-differences jolts)))

(defn joltage-children [jolts [n leaf]]
  (map #(cons n [%])
       (joltage-in-range leaf
                         (take 3 (drop-while #(>= leaf %)
                                             jolts)))))

(defn leaf-reducer [acc [n leaf]]
  (assoc acc leaf (+ (or (acc leaf) 0) n)))

(defn count-joltage-paths [jolts]
  (let [end? #(== (last jolts) (second %))]
    (loop [paths 0
           leaves '([1 0])]
      (if (empty? leaves)
        paths
        (recur (->> leaves
                    (filter end?)
                    (map first)
                    (cons paths)
                    (apply +))
               (mapcat
                (partial joltage-children jolts)
                (->> leaves
                     (remove end?)
                     (reduce leaf-reducer {})
                     (map reverse)
                     (sort-by second))))))))

(defn answer-one [jolts]
  (let [diffs (pair-diffs jolts)]
    (apply * (map count [(filter #(== 3 (second %)) diffs)
                         (filter #(== 1 (second %)) diffs)]))))
