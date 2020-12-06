(ns aoc-2020.day06
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.set :as set]))

(def input (slurp (io/reader (io/resource "input06"))))

(def customs-grammar
  (insta/parser
   {:answers (c/cat (c/nt :group) (c/star (c/cat (c/hide (c/nt :group-sep)) (c/nt :group))) (c/hide (c/nt :eof)))
    :group (c/cat (c/nt :person) (c/star (c/cat (c/hide (c/nt :person-sep)) (c/nt :person))))
    :person (c/plus (c/nt :letter))
    :letter (c/hide-tag (c/regexp "[a-z]"))
    :group-sep (c/hide-tag (c/string "\n\n"))
    :person-sep (c/hide-tag (c/string "\n"))
    :eof (c/hide-tag (c/cat (c/opt (c/string "\n")) (c/regexp "$")))}
   :start :answers))

(def answer-transform
  {:group set/intersection
   :person (fn [& answers] (set answers))
   :answers vector})

(defn sum-answer-counts [answers]
  (->> answers
       customs-grammar
       (insta/transform answer-transform)
       (map count)
       (apply +)))
