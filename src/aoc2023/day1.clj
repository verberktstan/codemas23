(ns aoc2023.day1
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(def digits1 (partial re-seq #"[\d.]"))

(defn- parse-digit [s]
  (let [idx (.indexOf ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] s)]
    (when-not (neg? idx) (str idx))))

(defn- digits2
  "Returns a sequence of parsed digits.
  e.g (digits2 'one2three') => [1 2 3]"
  [s]
  (->> s
       (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine))|[\d.]")
       (mapcat (partial remove str/blank?))
       (map #(or (parse-digit %) %))))

(defn- sum-digits
  "Returns the sum of all the digits found by function f in lines."
  [f lines]
  (->> lines
       (map f)
       (filter seq)
       (map (juxt first last))
       (map (partial apply str))
       (map edn/read-string)
       (apply +)))

(defn with-lines [file f]
  (with-open [reader (io/reader file)]
    (-> reader line-seq f)))

(t/deftest day1
  (t/are [result f file] (-> file (with-lines (partial sum-digits f)) (= result))
    142   digits1 "resources/day1a-testinput.txt"
    55816 digits1 "resources/day1a-input.txt"
    54980 digits2 "resources/day1a-input.txt"))

#_(t/run-tests) ; {:test 1, :pass 3, :fail 0, :error 0, :type :summary}
