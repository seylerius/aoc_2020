(ns aoc-2020.day01-1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def input (map edn/read-string (line-seq (io/reader (io/resource "input01")))))

(defn pair-reducer [seen n]
  (cond (set? seen) (if (seen (- 2020 n))
                      (* n (- 2020 n))
                      (conj seen n))
        (int? seen) seen))

(defn correct-product [input]
  (reduce pair-reducer #{} input))
