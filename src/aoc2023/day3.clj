(ns aoc2023.day3
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- position-pairs
  "Returns a map with the characters keyed by their x/y position.
  eg: `(position-pairs 'ab\ncd') => {[0 0] a, [0 1] b, [1 0] c, [1 1] d}`"
  [lines]
  (->> (for [row (-> lines count (range))
             col (-> lines first count (range))]
         (let [position [row col]
               character (-> (map seq lines) (nth row) (nth col))]
           [position character]))
       (into {})))

(defn- number-char? [character]
  (try (-> character str edn/read-string number?) (catch Exception _ nil)))

(defn- combine-numbers*
  "Returns sequence `coll` with a map conjoined with the combined :number and
  the :positions of the individual digits in the grid."
  [coll position-pairs]
  (cond-> coll
    (every? (comp number-char? second) position-pairs)
    (conj {:number    (apply (comp edn/read-string str) (map second position-pairs))
           :positions (map first position-pairs)})))

(def combine-numbers (partial reduce combine-numbers* nil))

(def DIRECTIONS
  [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])

(defn- part-number?
  "Returns a function that returns the number when it is adjacent to a symbol."
  [lookup]
  (fn part-number*
    [{:keys [positions number]}]
    (when (->> positions
               (mapcat #(map (comp lookup (partial mapv + %)) DIRECTIONS))
               (filter identity)
               (some (complement #{nil \. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9})))
      number)))

(defn- close-gear [lookup]
  (fn close-gear*
    [{:keys [positions] :as props}]
    (when-let [close (->> positions
                          (mapcat #(map (comp (partial find lookup) (partial mapv + %)) DIRECTIONS))
                          (filter identity)
                          (filter (comp #{\*} val))
                          seq)]
      (assoc props :close-gear (first close)))))

(defn- collect-numbers [lookup]
  (->> lookup
       (sort-by key)
       (partition-by (comp number-char? second))
       (partition-all 2)
       (mapcat combine-numbers)))

(defn- sum [predicate lookup numbers]
  (->> numbers (keep (predicate lookup)) (reduce +)))

(comment
  (with-open [reader (io/reader "resources/day3a-input.txt")]
    (let [lookup (->> reader line-seq position-pairs)
          numbers (collect-numbers lookup)]
      (sum part-number? lookup numbers))) ; 512794

  (with-open [reader (io/reader "resources/day3a-testinput.txt")]
    (let [lookup (->> reader line-seq position-pairs)
          numbers (->> lookup collect-numbers)]
      (->> numbers
           (keep (close-gear lookup))
           (group-by :close-gear)
           (filter (comp #(> % 1) count val)) ; Pick the ones where more than 1 number is adjacent to a gear.
           vals
           (map #(reduce * (map :number %))) ; Multiply the gear ratios
           (reduce +)))) ; 467835
  )
