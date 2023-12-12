(ns aoc2023.day5
  (:require [aoc2023.day1 :refer [with-lines]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]))

(def SEED? #"seeds\:[ \d]+")
(def MAPPING? #"[\S]+\-to\-[\S]+")

(defn- parse-ints [s]
  (some->> s (re-seq #"\d+") (map edn/read-string)))

(defn- parse-line [s]
  (let [seeds (some->> s (re-find SEED?) parse-ints)
        mapping (some->> s (re-find MAPPING?))
        ranges (parse-ints s)]
    (cond
      seeds   {:seeds seeds}
      mapping {:mapping (some-> mapping (str/split #"-to-"))}
      ranges  (zipmap [:destination-start :source-start :length] ranges))))

(defn- combine-mappings* [{:keys [mapping] :as result} {:keys [seeds length] :as m}]
  (cond
    seeds                m
    (and mapping length) (assoc result mapping m)
    (:mapping m)         (merge result m)
    :else                result))

(def combine-mappings (partial reduce combine-mappings* {}))

(comment
  (with-lines "resources/day5-testinput.txt" (comp combine-mappings (partial keep parse-line))))

(t/deftest day5a
  (t/are [result file] (= result (-> file (with-lines count)))
    33 "resources/day5-testinput.txt"))

#_(t/run-tests) ; {:error 0, :fail 0, :pass 4, :test 2, :type :summary}
