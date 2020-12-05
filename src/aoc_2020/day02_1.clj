(ns aoc-2020.day02-1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]))

(def input (line-seq (io/reader (io/resource "input02"))))

(def entry-regex #"(\d+)-(\d+) (\w): (\w+)$")

(def entry-grammar
  (insta/parser
   {:entry (c/cat (c/nt :number) (c/hide (c/string "-")) (c/nt :number)
                  (c/hide (c/nt :whitespace)) (c/nt :key) (c/hide (c/nt :rule-sep))
                  (c/hide (c/nt :whitespace)) (c/nt :password))
    :number (c/regexp "[0-9]+")
    :key (c/cat (c/nt :letter) (c/look (c/nt :rule-sep)))
    :rule-sep (c/hide-tag (c/string ":"))
    :whitespace (c/hide-tag (c/regexp "\\s+"))
    :password (c/plus (c/nt :letter))
    :letter (c/hide-tag (c/regexp "[a-z]"))} :start :entry))

(def entry-transform
  {:password vector
   :key identity
   :number edn/read-string
   :entry (fn [min max key password]
            (let [key-count (count (filter #{key} password))]
              (and (>= max key-count) (<= min key-count))))})

(defn new-valid-count [lines]
  (->> lines
       (map entry-grammar)
       (map #(insta/transform entry-transform %))
       (remove false?)
       count))
