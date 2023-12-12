(ns aoc2023.day5
  (:require [aoc2023.day1 :refer [with-lines]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :as t]
            [clojure.string :as str]))

(defn- parse-line [s]
  (let [seeds (some->> s (re-find #"seeds\:[ \d]+") (re-seq #"\d+") (map edn/read-string))
        mapping (some->> s (re-find #"[\S]+\-to\-[\S]+") #_(re-seq #"[\S]+\-to|to\-[\S]+"))
        ranges (some->> s (re-seq #"\d+") (map edn/read-string))]
    (if (or seeds mapping)
      {:seeds seeds
       :mapping (some-> mapping (str/split #"-to-"))}
      (zipmap [:destination-start :source-start :length] ranges))))

(comment
  (with-lines "resources/day5-testinput.txt" (partial mapv parse-line)))

(t/deftest day5a
  (t/are [result file] (= result (-> file (with-lines count)))
    33 "resources/day5-testinput.txt"))

#_(t/run-tests) ; {:error 0, :fail 0, :pass 4, :test 2, :type :summary}
