(ns aoc2023.day3
  (:require [aoc2023.day1 :refer [with-lines]]
            [clojure.edn :as edn]
            [clojure.test :as t]))

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

(defn- adjacent-gear [lookup]
  (fn adjacent-gear*
    [{:keys [positions] :as props}]
    (when-let [close (->> positions
                          (mapcat #(map (comp (partial find lookup) (partial mapv + %)) DIRECTIONS))
                          (filter identity)
                          (filter (comp #{\*} val))
                          seq)]
      (assoc props :adjacent-gear (first close)))))

(defn- collect-numbers [lookup]
  (->> lookup
       (sort-by key)
       (partition-by (comp number-char? second))
       (partition-all 2)
       (mapcat combine-numbers)))

(defn- sum [predicate lookup numbers]
  (->> numbers (keep (predicate lookup)) (reduce +)))

(defn- find-gear-ratios [lookup numbers]
  (->> numbers
       (keep (adjacent-gear lookup)) ; Keep the numbers that are adjacent to a gear
       (group-by :adjacent-gear) ; Group them by their adjacent gear
       (filter (comp pos? dec count val)) ; Pick the ones where more than 1 number is adjacent to a gear.
       vals))

(defn- multiply-and-sum-gear-ratios [gear-ratios]
  (->> gear-ratios
       (map #(reduce * (map :number %))) ; Multiply the gear ratios
       (reduce +)))

(t/deftest day3a
  (t/are [result file]
         (-> file
             (with-lines (comp #(sum part-number? % (collect-numbers %)) position-pairs))
             (= result))
    4361   "resources/day3a-testinput.txt"
    512794 "resources/day3a-input.txt"))

(t/deftest day3b
  (t/are [result file]
         (-> file
             (with-lines (comp multiply-and-sum-gear-ratios
                               #(->> % collect-numbers (find-gear-ratios %))
                               position-pairs))
             (= result))
    467835   "resources/day3a-testinput.txt"
    67779080 "resources/day3a-input.txt"))

#_(t/run-tests) ; {:test 2, :pass 4, :fail 0, :error 0, :type :summary} 
