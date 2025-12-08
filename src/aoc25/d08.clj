(ns aoc25.d08
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(def in (get-line-num))

(defn get-distance
  [[ax ay az] [bx by bz]]
  (+ (Math/pow (- ax bx) 2)
     (Math/pow (- ay by) 2)
     (Math/pow (- az bz) 2)))

(defn get-all-distances
  [pts]
  (loop [[pt & pts] (sort pts)
         ret []]
    (if (empty? pts)
      (sort ret)
      (recur pts
             (->> pts
                  (map (fn [a b] [(get-distance a b) a b])
                       (repeat pt))
                  (into ret))))))

(defn merge-circuit
  [cs & pts]
  (loop [[c & cs] cs
         ncs []
         nc #{}]
    (cond
      (empty? c)   (conj ncs nc)
      (some c pts) (recur cs ncs (into nc c))
      :else        (recur cs (conj ncs c) nc))))

(defn q1
  [pts n]
  (loop [[d & ds] (get-all-distances pts)
         cs (mapv hash-set pts)
         n n]
    (if (= n 0)
      (->> cs (map count) sort reverse (take 3) (apply *))
      (let [[_ a b] d
            ncs (merge-circuit cs a b)]
        (recur ds ncs (dec n))))))

(defn q2
  [pts]
  (loop [[d & ds] (get-all-distances pts)
         cs (mapv hash-set pts)]
    (let [[_ a b] d
          ncs (merge-circuit cs a b)]
      (if (= 1 (count ncs))
        (* (first a) (first b))
        (recur ds ncs)))))

#_(q1 in 1000)
#_(q2 in)
