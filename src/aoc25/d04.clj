(ns aoc25.d04
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(def in (get-line-chars))

(defn get-all-next
  [x y xs ys]
  (->> (for [ix [-1 0 1]
             iy [-1 0 1]
             :when (not= ix iy 0)]
         [(+ ix x) (+ iy y)])
       (filter
         (fn [[x y]]
           (and (<= 0 x (dec xs))
                (<= 0 y (dec ys)))))))

(defn check-removable
  [board x y xs ys]
  (and (= \@ (get-in board [x y]))
       (< (count
            (filter #(= \@ (get-in board %))
                    (get-all-next x y xs ys)))
          4)))

(defn q1
  [board]
  (let [xs (count board)
        ys (count (first board))]
    (count
      (for [x (range xs)
            y (range ys)
            :when (check-removable board x y xs ys)]
        [x y]))))


(defn q2
  [board]
  (let [xs (count board)
        ys (count (first board))]
    (loop [board board
           ret 0]
      (let [pts (for [x (range xs)
                      y (range ys)
                      :when (check-removable board x y xs ys)]
                  [x y])
            n (count pts)]
        (if (= n 0)
          ret
          (recur (reduce #(assoc-in %1 %2 \.) board pts)
                 (+ ret n)))))))

#_(q1 in)
#_(q2 in)

