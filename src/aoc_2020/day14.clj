(ns aoc-2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (line-seq (io/reader (io/resource "input14"))))

(defn apply-setmask [value [one-mask zero-mask]]
  (-> value
      (bit-and zero-mask)
      (bit-xor value)
      (bit-or one-mask)))

(defn value-mask-reducer [[one-mask zero-mask] bit]
  (case bit
    \X [(conj one-mask 0) (conj zero-mask 0)]
    \1 [(conj one-mask 1) (conj zero-mask 0)]
    \0 [(conj one-mask 0) (conj zero-mask 1)]))

(defn parse-mask-pair [pair]
  (map (comp edn/read-string (partial apply str)) pair))

(defn split-value-mask [mask]
  (->> mask
       (reduce value-mask-reducer [["2r"] ["2r"]])
       parse-mask-pair))

(defn instruction-elements [line]
  (let [[op _ value] (str/split line #" ")]
    [op value]))

(defn parse-instruction [line]
  (let [[op value] (instruction-elements line)]
    (if (= op "mask")
      (cons :mask (split-value-mask value))
      (let [[_ addr] (str/split op #"\[|\]")]
        (cons :mem (map edn/read-string [addr value]))))))

(defmulti init-reducer (fn [_ [op & _]] op))

(defmethod init-reducer :mask [[_ memory] [_ & mask]]
  [mask memory])

(defmethod init-reducer :mem [[mask memory] [_ addr value]]
  [mask (assoc memory addr (apply-setmask value mask))])

(defn add-setmask-bit [bit [one-mask zero-mask]]
  (case bit
    nil [(conj one-mask 0) (conj zero-mask 0)]
    1   [(conj one-mask 1) (conj zero-mask 0)]
    0   [(conj one-mask 0) (conj zero-mask 1)]))

(defn addr-mask-reducer [masks bit]
  (case bit
    \0 (map (partial add-setmask-bit nil) masks)
    \1 (map (partial add-setmask-bit 1) masks)
    \X (concat (map (partial add-setmask-bit 1) masks)
               (map (partial add-setmask-bit 0) masks))))

(defn split-addr-mask [mask]
  (->> mask
       (reduce addr-mask-reducer [[["2r"] ["2r"]]])
       (map parse-mask-pair)))

(defn parse-addr-masked-instruction [line]
  (let [[op value] (instruction-elements line)]
    (if (= op "mask")
      (cons :mask (split-addr-mask value))
      (let [[_ addr] (str/split op #"\[|\]")]
        (cons :mem (map edn/read-string [addr value]))))))

(defmulti addr-masked-init-reducer (fn [_ [op & _]] op))

(defmethod addr-masked-init-reducer :mask [[_ memory] [_ & masks]]
  [masks memory])

(defmethod addr-masked-init-reducer :mem [[masks memory] [_ addr value]]
  [masks (merge memory (into {} (map vector
                                     (map (partial apply-setmask addr) masks)
                                     (repeat value))))])
