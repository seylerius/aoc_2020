(ns aoc-2020.day05
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.edn :as edn]))

(def input (line-seq (io/reader (io/resource "input05"))))

(def seat-grammar
  (insta/parser
   {:seat (c/plus (c/ord (c/nt :row-bit) (c/nt :col-bit)))
    :row-bit (c/ord (c/string "F") (c/string "B"))
    :col-bit (c/ord (c/string "L") (c/string "R"))}
   :start :seat))

(def seat-transform
  {:row-bit (fn [bit] (if (= bit "F") "0" "1"))
   :col-bit (fn [bit] (if (= bit "L") "0" "1"))
   :seat (fn [& bits] (edn/read-string (apply str (cons "2r" bits))))
   })

(defn max-seat [seats]
  (->> seats
       (map seat-grammar)
       (map #(insta/transform seat-transform %))
       (apply max)))

(defn missing-seat [seats]
  (->> seats
       (map seat-grammar)
       (map #(insta/transform seat-transform %))
       sort
       (map-indexed (fn [i seat] [i seat]))
       (map (fn [[i seat]] [(+ 48 i) seat]))
       (partition-by (fn [[i seat]] (== i seat)))
       first
       last
       last
       inc))
