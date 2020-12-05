(ns aoc-2020.day02-2
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]))

(def input (line-seq (io/reader (io/resource "input02"))))

(def entry-regex #"(\d+)-(\d+) (\w): (\w+)$")

(def entry-grammar
  (insta/parser
   {:entry (c/cat (c/nt :pos1) (c/hide (c/string "-")) (c/nt :pos2)
                  (c/hide (c/nt :whitespace)) (c/nt :key) (c/hide (c/nt :rule-sep))
                  (c/hide (c/nt :whitespace)) (c/nt :password))
    :pos1 (c/nt :number)
    :pos2 (c/nt :number)
    :number (c/hide-tag (c/regexp "[0-9]+"))
    :key (c/cat (c/nt :letter) (c/look (c/nt :rule-sep)))
    :rule-sep (c/hide-tag (c/string ":"))
    :whitespace (c/hide-tag (c/regexp "\\s+"))
    :password (c/plus (c/nt :letter))
    :letter (c/hide-tag (c/regexp "[a-z]"))} :start :entry))

(def entry-transform
  {:password vector
   :key identity
   :pos1 (comp dec edn/read-string)
   :pos2 (comp dec edn/read-string)
   :entry (fn [pos1 pos2 key password]
            (let [pos1-char (nth password pos1)
                  pos2-char (nth password pos2)]
              (or (and (= key pos1-char)
                       (not (= key pos2-char)))
                  (and (= key pos2-char)
                       (not (= key pos1-char))))))})

(defn new-valid-count [lines]
  (->> lines
       (map entry-grammar)
       (map #(insta/transform entry-transform %))
       (remove false?)
       count))
