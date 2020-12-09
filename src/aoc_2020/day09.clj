(ns aoc-2020.day09
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def input (line-seq (io/reader (io/resource "input09"))))

(def numbers (map edn/read-string input))

(defn source-numbers [numbers i n]
  [(take-last 25 (take (+ i 25) numbers)) n])

(defn valid? [[sources n]]
  (some identity (for [[i x] (map-indexed vector (take 24 sources))
                       y (drop (inc i) sources)]
                   (== n (+ x y)))))

(def invalid? (comp not valid?))

(defn first-invalid [numbers]
  (->> numbers
       (drop 25)
       (map-indexed (partial source-numbers numbers))
       (filter invalid?)
       first
       second))

(defn sums-to-n? [n xs]
  (= n (apply + xs)))

(defn breach [numbers]
  (let [weakness (first-invalid numbers)
        weak-pos (.indexOf numbers weakness)
        sums? (partial sums-to-n? weakness)]
    (->> (for [step (range 1 weak-pos)
               i (range (- weak-pos step 1) step -1)
               j (range (inc i) (+ i step 1))
               :let [subseq (drop i (take (inc j) numbers))]
               :when (sums? subseq)]
           subseq)
         first
         ((fn [subseq] (+ (apply min subseq) (apply max subseq)))))))
