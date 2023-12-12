(ns aoc2023.day4
  (:require [aoc2023.day1 :refer [with-lines]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :as t]))

(defn- parse-card
  "Returns a map with the :card, :winning-nums and :my-nums parsed from string s"
  [s]
  {:card (some->> s (re-find #"Card [\d]+\:") (re-find #"\d+") edn/read-string)
   :winning-nums (some->> s (re-find #"\:[ \d]+\|") (re-seq #"\d+") set)
   :my-nums (some->> s (re-find #"\|[ \d]+") (re-seq #"\d+") set)})

(def my-winning-nums (comp (partial apply set/intersection) (juxt :winning-nums :my-nums)))

(defn- points [winning-nums]
  (when (seq winning-nums)
    (reduce * 1 (-> winning-nums count dec (repeat 2)))))

(defn- sum-card-points [lines]
  (->> lines (map parse-card) (map my-winning-nums) (keep points) (reduce +)))

(t/deftest day4a
  (t/are [result file] (= result (-> file (with-lines sum-card-points)))
    13     "resources/day4a-testinput.txt"
    21959 "resources/day4a-input.txt"))

#_(t/run-tests) ; {:test 1, :pass 2, :fail 0, :error 0, :type :summary} 

