(ns aoc25.d02
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(def in
  (mapv (fn [l]
          (mapv atoi (s/split l #"-")))
        (first (get-line-split ","))))

(defn q1 [l]
  (->> l
       (mapcat
         (fn [[si ei]]
           (filter
             (fn [n]
               (let [s (str n)]
                 (when (even? (count s))
                   (= (take (/ (count s) 2) s)
                      (drop (/ (count s) 2) s)))))
             (range si (inc ei)))))
       (apply +)))

#_(q1 in)

(defn q2 [l]
  (->> l
       (mapcat
         (fn [[si ei]]
           (filter
             (fn [n]
               (let [s (str n)]
                 (some (fn [i]
                         (let [pl (partition-all i s)]
                           (and (apply = pl) (> (count pl) 1))))
                       (range 1 (inc (/ (count s) 2))))))
             (range si (inc ei)))))
       (apply +)))

#_(q2 in)

