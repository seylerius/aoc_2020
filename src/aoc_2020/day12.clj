(ns aoc-2020.day12
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def input (line-seq (io/reader (io/resource "input12"))))

(def directions [:east :south :west :north])

(def dir-numbers {:east 0 :south 1 :west 2 :north 3})

(defn parse-instruction [[op & n]]
  (let [n (edn/read-string (apply str n))]
    (case op
      \F [:forward n]
      \N [:north n]
      \S [:south n]
      \E [:east n]
      \W [:west n]
      \R [:turn (/ n 90)]
      \L [:turn (/ n -90)])))

(defn parse-turn [heading turn]
  (nth directions (mod (+ turn (dir-numbers heading)) 4)))

(defn rotate-waypoint [waypoint turn]
  (loop [[east north] waypoint
         turn turn]
    (cond
      (zero? turn) [east north]
      (pos? turn) (recur [north (-' east)] (dec turn))
      (neg? turn) (recur [(-' north) east] (inc turn)))))

(defn turn-reducer [[heading instructions] [op n]]
  (case op
    (:north :south :east :west) [heading (conj instructions [op n])]
    :forward [heading (conj instructions [heading n])]
    :turn [(parse-turn heading n) instructions]))

(defn distance-reducer [[east north] [op n]]
  (case op
    :north [east (+ north n)]
    :south [east (- north n)]
    :east [(+ east n) north]
    :west [(- east n) north]))

(defn waypoint-reducer [[waypoint instructions] [op n]]
  (let [[east north] waypoint]
    (case op
      :turn [(rotate-waypoint waypoint n) instructions]
      :forward [waypoint (concat instructions (repeat n waypoint))]
      :north [[east (+ north n)] instructions]
      :south [[east (- north n)] instructions]
      :east [[(+ east n) north] instructions]
      :west [[(- east n) north] instructions])))

(defn position-reducer [[x y] [x1 y1]]
  [(+ x x1) (+ y y1)])

(defn manhattan [pos]
  (apply + (map math/abs pos)))
