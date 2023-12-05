(ns aoc2023.day1
  (:require [clojure.string :as str]))

(def test-input "1abc22
pqr3stu8vwx
a1b2c3d4e5f
unknown
treb7uchet")

(def test-input2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn- digits1 [coll]
  (re-seq #"[\d.]" coll))

(defn- parse-digit [s]
  (let [idx (.indexOf ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] s)]
    (when-not (neg? idx) (str idx))))

(defn- digits2 [coll]
  (->> coll
       (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine))|[\d.]")
       (mapcat (partial remove str/blank?))
       (map #(or (parse-digit %) %))))

(defn split-lines [s]
  (when s (str/split s #"\n")))

(defn sum-digits [f s]
  (let [coll (map f (split-lines s))]
    (->> coll
         (filter seq)
         (filter identity)
         (map #(vector (first %) (last %)))
         (map (partial apply str))
         (map read-string)
         (apply +))))

(comment
  (digits1 "1abc23")
  (digits1 "abc")
  (digits2 "one23")

  (parse-digit 1 #_"one")

  (sum-digits digits1 test-input)
  (sum-digits digits2 test-input2)

  (sum-digits digits1 (slurp "resources/day1a-input.edn")) ; => 55816 
  (sum-digits digits2 (slurp "resources/day1a-input.edn")) ; => 54980 
  )
