(ns aoc-2020.day08
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.edn :as edn]))

(def input (line-seq (io/reader (io/resource "input08"))))

(def asm-grammar
  (insta/parser
   {:inst (c/cat (c/nt :op) (c/hide (c/string " ")) (c/nt :arg))
    :op (c/regexp "[a-z]{3}")
    :arg (c/regexp "(\\+|-)[0-9]+")}
   :start :inst))

(def asm-transform
  {:arg edn/read-string
   :op keyword
   :inst vector})

(def bootcode (->> input
                   (map asm-grammar)
                   (map (fn [inst] (insta/transform asm-transform inst)))))

(defn exec-code [[seen changed?] code acc i]
  (let [seen (conj seen i)
        [op arg] (nth code i)]
    (cond
      (= op :jmp)
      (let [next (+ i arg)]
        (cond
          (contains? seen next) :looping
          (>= next (count code)) acc
          :else (let [result (exec-code [seen changed?] code acc next)]
                  (cond
                    (and (= result :looping)
                         (not changed?))
                    (let [next (inc i)
                          changed? true]
                      (if (>= next (count code)) acc
                          (exec-code [seen changed?] code acc next)))
                    (and (= result :looping) changed?) :looping
                    :else result))))
      (= op :acc) (let [next (inc i)
                        acc (+ acc arg)]
                    (cond
                      (contains? seen next) :looping
                      (>= next (count code)) acc
                      :else (exec-code [seen changed?] code acc next)))
      (= op :nop)
      (let [next (inc i)]
        (cond
          (contains? seen next) :looping
          (>= next (count code)) acc
          :else (let [result (exec-code [seen changed?] code acc next)]
                  (cond
                    (and (= result :looping)
                         (not changed?))
                    (let [next (+ i arg)
                          changed? true]
                      (if (>= next (count code)) acc
                          (exec-code [seen changed?] code acc next)))
                    (and (= result :looping) changed?) :looping
                    :else result)))))))
