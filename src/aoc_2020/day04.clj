(ns aoc-2020.day04
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [instaparse.combinators :as c]
            [clojure.set :as set]
            [clojure.edn :as edn]))

(def input (slurp (io/reader (io/resource "input04"))))

(def required-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(def required-validated-keys #{:birth :issue :expire :height :hair-color
                               :eye-color :passport-id})

(def passport-grammar
  (insta/parser
   {:stream (c/cat (c/nt :passport)
                   (c/star (c/cat (c/hide (c/nt :passport-sep)) (c/nt :passport)))
                   (c/hide (c/nt :eof)))
    :eof (c/hide-tag (c/cat (c/opt (c/string "\n")) (c/regexp "$")))
    :passport (c/cat (c/nt :field) (c/star (c/cat (c/hide (c/nt :field-sep)) (c/nt :field))))
    :passport-sep (c/hide-tag (c/regexp "\n\n"))
    :field-sep (c/hide-tag (c/regexp "[ \n]"))
    ;; :field (c/cat (c/nt :key) (c/nt :value))
    :field (c/ord (c/nt :birth) (c/nt :issue) (c/nt :expire) (c/nt :height)
                  (c/nt :hair-color) (c/nt :eye-color) (c/nt :passport-id)
                  (c/nt :country-id) (c/nt :invalid-field))
    :key (c/cat (c/ord (c/string "byr") (c/string "iyr") (c/string "eyr")
                       (c/string "hgt") (c/string "hcl") (c/string "ecl")
                       (c/string "pid") (c/string "cid"))
                (c/hide (c/string ":")))
    :value (c/regexp "[a-z0-9#]+")
    :invalid-field (c/cat (c/nt :key) (c/nt :value))
    :birth (c/cat (c/hide (c/string "byr:")) (c/nt :number-4d))
    :issue (c/cat (c/hide (c/string "iyr:")) (c/nt :number-4d))
    :expire (c/cat (c/hide (c/string "eyr:")) (c/nt :number-4d))
    :height (c/cat (c/hide (c/string "hgt:"))
                   (c/ord (c/cat (c/nt :number-3d) (c/string "cm"))
                          (c/cat (c/nt :number-2d) (c/string "in"))))
    :hair-color (c/cat (c/hide (c/string "hcl:")) (c/nt :hex-color))
    :eye-color (c/cat (c/hide (c/string "ecl:"))
                      (c/ord (c/string "amb") (c/string "blu") (c/string "brn")
                             (c/string "gry") (c/string "grn") (c/string "hzl")
                             (c/string "oth")))
    :passport-id (c/cat (c/hide (c/string "pid:")) (c/nt :number-9d))
    :country-id (c/cat (c/hide (c/string "cid:")) (c/nt :value))
    :number-9d (c/cat (c/nt :digit-dec) (c/nt :digit-dec) (c/nt :digit-dec)
                      (c/nt :digit-dec) (c/nt :digit-dec) (c/nt :digit-dec)
                      (c/nt :digit-dec) (c/nt :digit-dec) (c/nt :digit-dec))
    :number-4d (c/cat (c/nt :digit-dec) (c/nt :digit-dec)
                      (c/nt :digit-dec) (c/nt :digit-dec))
    :number-3d (c/cat (c/nt :digit-dec) (c/nt :digit-dec) (c/nt :digit-dec))
    :number-2d (c/cat (c/nt :digit-dec) (c/nt :digit-dec))
    :hex-color (c/cat (c/string "#") (c/nt :hex-6d))
    :hex-6d (c/hide-tag (c/cat (c/nt :digit-hex) (c/nt :digit-hex) (c/nt :digit-hex)
                               (c/nt :digit-hex) (c/nt :digit-hex) (c/nt :digit-hex)))
    :digit-dec (c/hide-tag (c/regexp "[0-9]"))
    :digit-hex (c/hide-tag (c/regexp "[0-9a-f]"))} :start :stream))

(defn correct-keys? [passport]
  (->> passport
       (filter second)
       (into {})
       keys
       set
       (set/difference required-validated-keys)
       empty?))

(def passport-transform
  {:stream (fn [& passports] (map correct-keys? passports))
   :passport (fn [& field-pairs] (into {} field-pairs))
   :field identity
   :key keyword
   :invalid-field (fn [& items] [:invalid-field items])
   :birth (fn [year] [:birth (if (and (<= 1920 year) (>= 2002 year)) year)])
   :issue (fn [year] [:issue (if (and (<= 2010 year) (>= 2020 year)) year)])
   :expire (fn [year] [:expire (if (and (<= 2020 year) (>= 2030 year)) year)])
   :height (fn [height unit]
             [:height (if (or (and (= unit "cm") (<= 150 height) (>= 193 height))
                              (and (= unit "in") (<= 59 height) (>= 76 height)))
                        [height unit])])
   :number-9d str
   :number-4d (comp edn/read-string str)
   :number-3d (comp edn/read-string str)
   :number-2d (comp edn/read-string str)
   :hex-color str
   :value identity})

(defn count-valid-passports [passport-stream]
  (->> passport-stream
       passport-grammar
       (insta/transform passport-transform)
       (remove false?)
       count))

;; (defn get-string-entries []
;;   (remove #(empty? (first %)) (partition-by str/blank? *input*)))

;; (defn split-fields [entry]
;;   (apply concat (map #(str/split % #"\s+") entry)))

;; (defn split-keys [entry]
;;   (apply concat (map #(str/split % #":") entry)))

;; (defn read-entry [entry]
;;   (apply hash-map (map-indexed (fn [i item] (if (even? i) (keyword item) item)) entry)))


;; (defn valid? [passport]
;;   (and 
;;    (empty? (set/difference required-keys (set (keys passport))))
;;    (let [{:keys [byr eyr iyr hgt hcl ecl pid]} passport
;;          [birth expiration issue] (map edn/read-string [byr eyr iyr])
;;          [h u] (rest (re-find #"^(\d+)(\w+)$" hgt))
;;          [height unit] [(edn/read-string h) (keyword u)]
;;          hair-color (second (re-find #"^#([a-f0-9]{6})$" hcl))
;;          eye-color (second (re-find #"^(amb|blu|brn|gry|grn|hzl|oth)$" ecl))
;;          passport-id (re-find #"^\d{9}$" pid)]
;;      (and
;;       birth expiration issue height unit hair-color eye-color passport-id
;;       (<= 1920 birth) (>= 2002 birth)
;;       (<= 2020 expiration) (>= 2030 expiration)
;;       (<= 2010 issue) (>= 2020 issue)
;;       (cond (= unit :cm) (and (<= 150 height) (>= 193 height))
;;             (= unit :in) (and (<= 59 height) (>= 76 height))
;;             :else nil)))))

;; (->> (get-string-entries)
;;      (map split-fields)
;;      (map split-keys)
;;      (map read-entry)
;;      (filter valid?)
;;      count)
