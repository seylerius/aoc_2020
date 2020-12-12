(ns aoc-2020.day01-2
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def input (map edn/read-string (line-seq (io/reader (io/resource "input01")))))

(defn correct-product [numbers]
  (let [after (fn [n ns] (rest(second (split-with #(not (= n %)) ns))))
        pairs (apply concat (map (fn [a] (map (fn [b] (conj [a] b)) (after a numbers))) numbers))
        trios (apply concat (map (fn [a] (map (fn [b] (conj a b)) (after (second  a) numbers))) pairs))
        correct-trio (first (filter (fn [trio] (= 2020 (apply + trio))) trios))]
    (apply * correct-trio)))
