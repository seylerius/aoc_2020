(ns aoc-2020.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp (io/reader (io/resource "input06"))))

(defn all-group-answers [answers]
  (->> (str/split answers #"\n\n")
       (map #(str/replace % #"\n" ""))
       (map set)
       (map count)
       (apply +)))

(defn agreed-group-answers [answers]
  (->> (str/split answers #"\n\n")
       (map #(str/split % #"\n"))
       (map #(map set %))
       (map #(apply set/intersection %))
       (map count)
       (apply +)))
