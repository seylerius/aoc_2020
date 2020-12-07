(ns aoc-2020.day07
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (line-seq (io/reader (io/resource "input07"))))

(def bag-grammar
  (insta/parser
   {:rule (c/hide-tag (c/cat (c/nt :bag) (c/hide (c/string " ")) (c/nt :contents)))
    :contents (c/cat (c/hide (c/string "contain "))
                     (c/ord (c/nt :empty)
                            (c/cat (c/nt :bag)
                                   (c/star (c/cat (c/hide (c/nt :content-sep))
                                                  (c/nt :bag)))))
                     (c/hide (c/string ".")))
    :bag (c/cat (c/opt (c/cat (c/nt :number) (c/hide (c/string " "))))
                (c/regexp "[a-z]+ [a-z]+") (c/hide (c/regexp " bags?")))
    :number (c/regexp "[0-9]+")
    :content-sep (c/hide-tag (c/string ", "))
    :empty (c/string "no other bags")}
   :start :rule))

(def rule-transform
  {:bag (fn ([n bag] (take n (repeat (keyword (str/replace bag #" " "-")))))
          ([bag] (keyword (str/replace bag #" " "-"))))
   :contents (fn [& bags] (remove nil? (apply concat bags)))
   :number edn/read-string
   :empty (fn [_] nil)})

(defn parse-rules [input]
  (->> input
       (map bag-grammar)
       (map #(insta/transform rule-transform %))
       (apply concat)
       (apply hash-map)))

(defn contains-bag? [rules target container]
  (cond
    (contains? (rules container) target) container
    (some identity (map (partial contains-bag? rules target)
                          (rules container))) container
    :else nil))

(defn containers-for [rules target]
  (->> (keys rules)
       (map (fn [bag] (trampoline contains-bag? rules target bag)))
       (remove nil?)
       set))

(defn count-bags [rules bag]
  (let [contents (into [] (rules bag))]
    (if (empty? contents) 0
        (apply + (map #(inc (count-bags rules %)) contents)))))

(defn count-contents [rules bag]
  (trampoline count-bags rules bag))
