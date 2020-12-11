(ns aoc-2020.day11
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "input11"))))

(defn parse-seat [seat]
  (cond (= seat \.) nil
        (= seat \L) false
        (= seat \#) true))

(defn parse-row [row]
  (map parse-seat row))

(defn parse-layout [input]
  (map parse-row input))

(defn get-seat [layout [row col]]
  (nth (nth layout row) col))

(defn index-seat [i j seat]
  [seat [i j]])

(defn index-row [i row]
  (map-indexed (partial index-seat i) row))

(defn index-layout [layout]
  (map-indexed index-row layout))

(def adjacency   [[-1 -1] [-1  0] [-1  1]
                  [0  -1]         [0   1]
                  [1  -1] [1   0] [1   1]])

(defn get-north-los [layout row col]
  (let [seen? (->> (range (dec row) -1 -1)
                   (map #(get-seat layout [% col]))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn get-south-los [layout row col]
  (let [seen? (->> (range (inc row) (count layout))
                   (map #(get-seat layout [% col]))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn get-east-los [layout row col]
  (let [seen? (->> (range (inc col) (count (first layout)))
                   (map #(get-seat layout [row %]))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn get-west-los [layout row col]
  (let [seen? (->> (range (dec col) -1 -1)
       (map #(get-seat layout [row %]))
       (map first)
       (remove nil?)
       first)]
    (if seen? 1 0)))

(defn get-northeast-los [layout row col]
  (let [seen? (->> (map vector
                        (range (dec row) -1 -1)
                        (range (inc col) (count (first layout))))
                   (map (fn [pos] (get-seat layout pos)))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn get-southeast-los [layout row col]
  (let [seen? (->> (map vector
                        (range (inc row) (count layout))
                        (range (inc col) (count (first layout))))
                   (map (fn [pos] (get-seat layout pos)))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn get-northwest-los [layout row col]
  (let [seen? (->> (map vector
                        (range (dec row) -1 -1)
                        (range (dec col) -1 -1))
                   (map (fn [pos] (get-seat layout pos)))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn get-southwest-los [layout row col]
  (let [seen? (->> (map vector
                        (range (inc row) (count layout))
                        (range (dec col) -1 -1))
                   (map (fn [pos] (get-seat layout pos)))
                   (map first)
                   (remove nil?)
                   first)]
    (if seen? 1 0)))

(defn count-sightlines [layout row col]
  (->> [get-north-los
        get-south-los
        get-east-los
        get-west-los
        get-northeast-los
        get-southeast-los
        get-northwest-los
        get-southwest-los]
       (map #(% layout row col))
       (filter number?)
       (apply +)))

(defn count-adjacent-seats [layout row col]
  (let [length (count layout)
        width (count (first layout))]
    (->> adjacency
         (map (fn [[r c]] [(+ r row) (+ c col)]))
         (remove #(some neg? %))
         (remove #(<= length (first %)))
         (remove #(<= width (second %)))
         (map (partial get-seat layout))
         (map first)
         (filter true?)
         count)))

(defn change-seat [layout [seat [row col]]]
  (let [neighbors (count-adjacent-seats layout row col)]
    (cond (nil? seat) [false [nil [row col]]]
          (and (false? seat)
               (zero? neighbors)) [true [true [row col]]]
          (and (true? seat)
               (>= neighbors 4)) [true [false [row col]]]
          :else [false [seat [row col]]])))

(defn row-change-reducer [old-layout [old-change row] seat]
  (let [[new-change? new-seat] (change-seat old-layout seat)
        changed? (or new-change? old-change)
        new-row (conj row new-seat)]
    [changed? new-row]))

(defn layout-change-reducer [old-layout [old-change layout] row]
  (let [[changed? new-row] (reduce (partial row-change-reducer old-layout)
                                   [old-change []] row)
        new-layout (conj layout new-row)]
    [changed? new-layout]))

(defn stabilize-layout [layout]
  (let [changed? (atom true)
        final (atom layout)]
    (while @changed?
      (let [[new-change new-layout] (reduce (partial layout-change-reducer @final) [false []] @final)]
        (reset! changed? new-change)
        (reset! final new-layout)))
    @final))

(defn change-seat-los [layout [seat [row col]]]
  (let [neighbors (count-sightlines layout row col)]
    (cond (nil? seat) [false [nil [row col]]]
          (and (false? seat)
               (zero? neighbors)) [true [true [row col]]]
          (and (true? seat)
               (>= neighbors 5)) [true [false [row col]]]
          :else [false [seat [row col]]])))

(defn row-change-los-reducer [old-layout [old-change row] seat]
  (let [[new-change? new-seat] (change-seat-los old-layout seat)
        changed? (or new-change? old-change)
        new-row (conj row new-seat)]
    [changed? new-row]))

(defn layout-change-los-reducer [old-layout [old-change layout] row]
  (let [[changed? new-row] (reduce (partial row-change-los-reducer old-layout)
                                   [old-change []] row)
        new-layout (conj layout new-row)]
    [changed? new-layout]))

(defn stabilize-los-layout [layout]
  (let [changed? (atom true)
        final (atom layout)]
    (while @changed?
      (let [[new-change new-layout] (reduce (partial layout-change-reducer @final) [false []] @final)]
        (reset! changed? new-change)
        (reset! final new-layout)))
    @final))

(defn count-row [row]
  (apply + (map (fn [[seat _]] (if seat 1 0)) row)))

(defn count-layout [layout]
  (apply + (map count-row layout)))
