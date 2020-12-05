(ns aoc-2020.day05
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "input05"))))

(defn seat-to-number [seat]
  (edn/read-string
   (str "2r" (-> seat
                 (str/replace "F" "0")
                 (str/replace "L" "0")
                 (str/replace "R" "1")
                 (str/replace "B" "1")))))

(defn max-seat [seats]
  (->> seats
       (map seat-to-number)
       (apply max)))

(defn missing-seat [seats]
  (->> seats
       (map seat-to-number)
       sort
       (map-indexed (fn [i seat] [i seat]))
       (map (fn [[i seat]] [(+ 48 i) seat]))
       (partition-by (fn [[i seat]] (== i seat)))
       first
       last
       last
       inc))
