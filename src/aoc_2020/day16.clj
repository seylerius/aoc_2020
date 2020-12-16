(ns aoc-2020.day16
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def input (slurp (io/resource "input16")))

(def grammar
  (insta/parser
   {:rule      (c/cat (c/nt :rule-name) (c/nt :ranges))
    :rule-name (c/cat (c/nt :word)
                      (c/star (c/cat (c/nt :space) (c/nt :word)))
                      (c/hide (c/string ": ")))
    :space     (c/string " ")
    :word      (c/hide-tag (c/regexp "[a-z]+"))
    :ranges    (c/cat (c/nt :range) (c/star (c/cat (c/hide (c/string " or ")) (c/nt :range))))
    :range     (c/cat (c/nt :number) (c/hide (c/string "-")) (c/nt :number))
    :number    (c/regexp "[0-9]+")
    :rules     (c/cat (c/nt :rule) (c/star (c/cat (c/nt :newline) (c/nt :rule))) (c/plus (c/nt :newline)))
    :newline   (c/hide-tag (c/hide (c/string "\n")))
    :ticket    (c/cat (c/nt :number) (c/star (c/cat (c/hide (c/string ",")) (c/nt :number))))
    :my-ticket (c/cat (c/hide (c/string "your ticket:\n")) (c/nt :ticket) (c/plus (c/nt :newline)))
    :nearby    (c/cat (c/hide (c/string "nearby tickets:\n")) (c/plus (c/cat (c/nt :ticket) (c/nt :newline))))
    :input     (c/cat (c/nt :rules) (c/nt :my-ticket) (c/nt :nearby))}
   :start :input))

(defn split-element [[k & vals]]
  [k (if (== 1 (count vals))
       (first vals)
       vals)])

(def transform
  {:number    edn/read-string
   :space     (fn [_] "-")
   :rule-name (fn [& words] (keyword (apply str words)))
   :range     (fn [a b] (range a (inc b)))
   :ranges    (comp set concat)
   :rule      vector
   :ticket    vector
   :rules     (comp (partial conj [:rules]) (partial into {}) vector)
   :input     (fn [& kvals] (->> kvals
                                (map split-element)
                                (into {})))})

(defn parse [input]
  (->> input
       grammar
       (insta/transform transform)))

(defn any-rule? [rules]
  (apply set/union (vals rules)))

(defn invalid-ticket? [rule ticket]
  (seq (remove rule ticket)))

(defn sum-invalid-values [input]
  (let [rules           (:rules input)
        nearby          (:nearby input)
        invalid-ticket? (partial invalid-ticket? (any-rule? rules))]
    (->> nearby
         (keep invalid-ticket?)
         flatten
         (apply +))))

(defn identifier [columns idents [name rule]]
  (let [column-test (fn [[i col]] (if (every? rule col) i nil))]
    (assoc idents name (keep column-test columns))))

(defn refine-idents [idents]
  (loop [idents idents]
    (if (seq (filter coll? (vals idents)))
      (let [[field col] (first (keep (fn [[field cols]]
                                       (if (and (coll? cols) (== 1 (count cols)))
                                         [field (first cols)]
                                         nil))
                                     idents))
            idents (into {} (map (fn [[field cols]]
                                   [field (if (number? cols)
                                            cols
                                            (remove #{col} cols))])
                                 idents))]
        (recur (assoc idents field col)))
      (map second (sort-by first (map reverse idents))))))

(defn parse-my-ticket [input]
  (let [data            (parse input)
        rules           (:rules data)
        nearby          (:nearby data)
        ticket          (:my-ticket data)
        invalid-ticket? (partial invalid-ticket? (any-rule? rules))
        valid-tickets   (remove invalid-ticket? nearby)
        columns         (map-indexed vector (apply mapv vector valid-tickets))
        possible-fields (reduce (partial identifier columns) {} rules)
        fields          (refine-idents possible-fields)
        my-ticket       (into {} (map (fn [field v] [field v]) fields ticket))]
    my-ticket))

(def answer-fields [:departure-location
                    :departure-station
                    :departure-platform
                    :departure-track
                    :departure-date
                    :departure-time])

(defn answer [ticket]
  (apply * (map #(% ticket) answer-fields)))
