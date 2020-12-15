(ns aoc-2020.day15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (mapv edn/read-string (str/split "19,20,14,0,9,1" #",")))

(defn saw-number [seen n turn]
  (assoc seen n (lazy-seq (cons turn (seen n)))))

(defn next-number [seen prev]
  (let [last-turn (first (seen prev))
        prev-turn (or (second (seen prev)) last-turn)]
    (- last-turn prev-turn)))

(defn turn-reducer [[seen prev] turn]
  (cond
    (and (coll? prev)
         (seq (rest prev))) [(saw-number seen (first prev) turn)
                             (rest prev)]
    (coll? prev) [(saw-number seen (first prev) turn) (first prev)]
    :else (let [new-val (next-number seen prev)]
            [(saw-number seen new-val turn) new-val])))

(defn recited-on [input turn]
  (second (reduce turn-reducer [{} input] (range turn))))
