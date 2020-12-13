(ns aoc-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def input (slurp (io/resource "input13")))

(defn parse-bus [[i bus]]
  [i (edn/read-string bus)])

(defn invalid-bus? [[i bus]]
  (= bus "x"))

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s  0
                     s0 1
                     t  1
                     t0 0
                     r  (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))
 
(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
        ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

(defn parse-contest-schedule [input]
  (let [bus-string (second (str/split-lines input))
        timed-busses (map parse-bus
                          (remove invalid-bus?
                                  (map-indexed vector
                                               (str/split bus-string #","))))
        busses (map second timed-busses)
        a (map (fn [[i bus]]
                 (mod
                  (- (+ bus i))
                  bus))
               timed-busses)]
    (prn busses)
    (prn a)
    (chinese-remainder busses a)))

(defn parse-schedule [input]
  (let [[start-string bus-string] (str/split-lines input)
        start (edn/read-string start-string)
        busses (map edn/read-string (remove #{"x"} (str/split bus-string #",")))]
    [start busses]))

(defn immediacy [start bus]
  (- bus (mod start bus)))

(defn next-bus [start busses]
  (first (sort-by (partial immediacy start) busses)))

(defn bus-wait [start bus]
  (- (* (inc (quot start bus)) bus) start))

(defn answer-one [input]
  (let [[start busses] (parse-schedule input)
        bus (next-bus start busses)
        wait (bus-wait start bus)]
    (* bus wait)))
