(ns aoc25.d01
  (:require [aoc25.utils :refer :all]))

(defn parse-input [ll]
  (mapv (fn [[d & l]]
          [(if (= d \L) - +)
           (atoi (apply str l))])
        ll))

(def in (parse-input (get-line-chars)))

(defn q1 [l]
  (reduce
    (fn [[cur ret] [op dial]]
      (let [nv (mod (op cur dial) 100)]
        (if (= 0 nv)
          [0 (inc ret)]
          [nv ret])))
    [50 0] l))

(defn q2 [l]
  (reduce
    (fn [[cur ret] [op dial]]
      (reduce
        (fn [[cur ret] op]
          (let [nv (mod (op cur 1) 100)]
            (if (= 0 nv)
              [0 (inc ret)]
              [nv ret])))
        [cur ret] (repeat dial op)))
    [50 0] l))

#_(q1 in)
#_(q2 in)
