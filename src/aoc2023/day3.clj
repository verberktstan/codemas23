(ns aoc2023.day3
  (:require [clojure.string :as str]))

(def TESTINPUT
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn- pivot
  "Returns a map with characters from the lines keyed by their [row-index column-index].
  eg: `(pivot ['ab' 'cd']) => {[0 0] a, [0 1] b, [1 0] c, [1 1] d}`"
  [lines]
  (apply
   merge
   (for [row-idx (-> lines count (range)) ; for every row..
         col-idx (-> lines first count (range))] ; ..and every col
     (let [row       (nth lines row-idx)
           character (nth row   col-idx)]
       {[row-idx col-idx] character})))) ; save the car by row- and column-index.

(def DIRECTIONS
  {:north     [-1 0] :northeast [-1 1] :east [0 1]
   :southeast [1 1]  :south     [1 0]  :southwest [1 -1]
   :west      [0 -1] :northwest [-1 -1]})

(defn- adjacent-characters
  "Returns a sequence of maps, one for each coordinate, with an adjacent symbol associated with it's :direction"
  [pivoted-map]
  (for [[k] pivoted-map]
    (reduce-kv
     (fn [m direction pos]
       (let [pos'       (mapv + k pos)
             adjacent   (get pivoted-map pos')
             symbol-map {:pos pos' :symbol adjacent :direction direction}]
         (cond-> m
           adjacent (update :adjacent conj symbol-map))))
     {:pos k}
     DIRECTIONS)))

(def DOT-OR-INT? #{\. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn- part-number?
  "Returns a truethy value if one of the adjacent symbols is not a dot or an int."
  [{:keys [adjacent]}]
  (some (complement DOT-OR-INT?) (map :symbol adjacent)))

(comment
  (let [lines (str/split TESTINPUT #"\n")]
    (->> lines
         pivot
         adjacent-characters
         (filter part-number?))))
