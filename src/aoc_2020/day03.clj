(ns aoc-2020.day03
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]))

(def input (line-seq (io/reader (io/resource "input03"))))

(def row-length (count (first input)))

(def slopes [1 3 5 7 0.5])

(def hill-grammar
  (insta/parser
   {:row (c/plus (c/nt :square))
    :square (c/hide-tag (c/ord (c/nt :clear) (c/nt :tree)))
    :tree (c/string "#")
    :clear (c/string ".")} :start :row))

(def hill-transform
  {:row vector
   :tree (fn [_] true)
   :clear (fn [_] false)})

(defn count-impacts [rows slope]
  (->> rows
       (map hill-grammar)
       (map #(insta/transform hill-transform %))
       (map-indexed (fn [i row] [(* i slope) row]))
       (filter (fn [[n row]] (== n (int n))))
       (map (fn [[n row]] [(mod (int n) row-length) row]))
       (map (fn [[n row]] (nth row n)))
       (remove false?)
       count))

(defn product-of-slopes [rows slopes]
  (apply * (map (partial count-impacts rows) slopes)))
