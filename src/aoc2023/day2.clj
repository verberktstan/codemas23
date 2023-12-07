(ns aoc2023.day2
  (:require [clojure.string :as str]))

(def TESTINPUT
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn- split-game-data [s]
  (some-> s (str/split #": ")))

;; Some Regexes
(def DIGITS #"[\d.]+")
(def LETTERS #"[a-z]+")

(defn- parse-game [[game data]]
  {:game (read-string (re-find DIGITS game))
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
  [m {:keys [colors] :as game-data}]
  (assoc game-data :possible-colors (map (partial merge-with - m) colors)))

(defn- impossible-colors [{:keys [possible-colors] :as m}]
  (assoc m :impossible-colors (filter (partial some (comp neg? val)) possible-colors)))

(defn- prepare-colors [s]
  (->>
   (str/split s #"\n") ; Split lines (by newline char)
   (map split-game-data)
   (map parse-game)
   (map enrich-colors)))

(defn- sum-possible-game-ids
  [s]
  (->> s
       prepare-colors
       (map (partial possible-colors {"red" 12 "green" 13 "blue" 14})) ; Compare possible colors to n red, green and blue cubes
       (map impossible-colors)
       (remove (comp seq :impossible-colors)) ; Remove impossible colors
       (map :game) ; Find the game id's
       (reduce +))) ; Sum those id's

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

(comment
  (sum-possible-game-ids TESTINPUT) ; => 8
  (sum-possible-game-ids (slurp "resources/day2a-input.edn")) ; => 1931
  (sum-powers TESTINPUT) ; => 2286
  (sum-powers (slurp "resources/day2a-input.edn")) ; => 83105
  )
