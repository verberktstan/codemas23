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

(defn- points [winning-nums]
  (when (seq winning-nums)
    (reduce * 1 (-> winning-nums count dec (repeat 2)))))

(def my-winning-nums* (comp (partial apply set/intersection) (juxt :winning-nums :my-nums)))

(defn my-winning-nums [card]
  (let [mwn  (my-winning-nums* card)
        pnts (points mwn)]
    (cond-> (assoc card :win-count (count mwn) :points 0)
      pnts      (assoc :points pnts)
      (seq mwn) (assoc :my-winning-nums mwn))))

(defn- sum-card-points [lines]
  (->> lines (map parse-card) (map my-winning-nums*) (keep points) (reduce +)))

(defn- duplicate-winning-card-frequencies [lines]
  (let [cards (->> lines (map parse-card) (map my-winning-nums))]
    (reduce
     (fn [frequencies {:keys [card win-count]}]
       (let [cards (when (pos? win-count)
                     (->> (range (inc card) (-> card (+ win-count) inc))
                          (repeat (get frequencies card))
                          (mapcat identity)))]
         (reduce (fn [m card] (cond-> m card (update card inc))) frequencies cards)))
     (frequencies (map :card cards))
     cards)))

(defn- sum-frequencies [frequencies]
  (->> frequencies vals (reduce +)))

(t/deftest day4a
  (t/are [result file] (= result (-> file (with-lines sum-card-points)))
    13    "resources/day4a-testinput.txt"
    21959 "resources/day4a-input.txt"))

(t/deftest day4b
  (t/are [result file] (= result (-> file (with-lines (comp sum-frequencies duplicate-winning-card-frequencies))))
    30 "resources/day4a-testinput.txt"
    #_#_3110653 "resources/day4a-input.txt"))

#_(t/run-tests) ; {:test 2, :pass 3, :fail 0, :error 0, :type :summary} 
