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

(defn- color->map [s]
  (let [n     (->> s (re-find DIGITS) read-string)
        color  (re-find LETTERS s)]
    {color n}))

(defn- parse-data
  "Returns map `m` with the counts of the colors associated with :colors."
  [{:keys [data] :as m}]
  (let [reveals (str/split data #"; ")
        draws   (map #(str/split % #", ") reveals)
        colors  (map (comp (partial reduce into) (partial map color->map)) draws)]
    (merge m {:reveals reveals :draws draws :colors colors})))

(defn- possible-colors
  "Returns game-data map with :possible-colors, where color counts are subtracted from the given colors map `m`."
  [m {:keys [colors] :as game-data}]
  (assoc game-data :possible-colors (map (partial merge-with - m) colors)))

(defn- impossible-colors [{:keys [possible-colors] :as m}]
  (assoc m :impossible-colors (filter (partial some (comp neg? val)) possible-colors)))

(comment
  (->>
   (str/split TESTINPUT #"\n")
   (map split-game-data)
   (map parse-game)
   (map parse-data)
   (map (partial possible-colors {"red" 12 "green" 13 "blue" 14}))
   (map impossible-colors)
   (remove (comp seq :impossible-colors))
   (map :game)
   (reduce +)))
