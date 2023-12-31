(ns aoc2023.day2
  (:require [aoc2023.day1 :refer [with-lines]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn- split-game-data [s]
  (some-> s (str/split #": ")))

;; Some Regexes
(def DIGITS #"[\d.]+")
(def LETTERS #"[a-z]+")

(defn- parse-game [[game data]]
  {:game (edn/read-string (re-find DIGITS game))
   :data data})

(defn- color-frequencies [s]
  (let [n     (->> s (re-find DIGITS) read-string)
        color (re-find LETTERS s)]
    {color n}))

(defn- enrich-colors
  "Returns map `m` with the frequencies of the colors associated with :colors."
  [{:keys [data] :as m}]
  (let [reveals (str/split data #"; ")
        draws   (map #(str/split % #", ") reveals)
        colors  (map (comp (partial reduce into)
                           (partial map color-frequencies))
                     draws)]
    (assoc m :colors colors)))

(defn- possible-colors
  "Returns game-data map with :possible-colors, where color counts are subtracted from the given colors map `m`."
  [{:keys [colors] :as game-data}]
  (let [m {"red" 12 "green" 13 "blue" 14}]
    (assoc game-data :possible-colors (map (partial merge-with - m) colors))))

(defn- impossible-colors [{:keys [possible-colors] :as m}]
  (assoc m :impossible-colors (filter (partial some (comp neg? val)) possible-colors)))

(defn- prepare-colors [lines]
  (->> lines
       (map split-game-data)
       (map parse-game)
       (map enrich-colors)))

(defn- sum-possible-game-ids
  [lines]
  (->> lines
       prepare-colors
       (map possible-colors) ; Compare possible colors to n red, green and blue cubes
       (map impossible-colors)
       (remove (comp seq :impossible-colors)) ; Remove impossible colors
       (map :game) ; Find the game id's
       (reduce +))) ; Sum those id's

(t/deftest day2a
  (t/are [result file] (-> file (with-lines sum-possible-game-ids) (= result))
    8    "resources/day2a-testinput.txt"
    1931 "resources/day2a-input.txt"))

(defn- find-minimum-cubes [{:keys [colors] :as m}]
  (assoc m
    :minimum-cubes
    (apply merge-with max colors)))

(defn- multiply-powers [{:keys [minimum-cubes] :as m}]
  (assoc m
    :power
    (reduce-kv (fn [x _ y] (* x y)) 1 minimum-cubes)))

(defn- sum-powers
  [s]
  (->> s
       prepare-colors
       (map find-minimum-cubes)
       (map multiply-powers)
       (map :power)
       (reduce + 0)))

(t/deftest day2b
  (t/are [result file] (-> file (with-lines sum-powers) (= result))
    2286  "resources/day2a-testinput.txt"
    83105 "resources/day2a-input.txt"))

#_(t/run-tests) ; {:test 2, :pass 4, :fail 0, :error 0, :type :summary} 

