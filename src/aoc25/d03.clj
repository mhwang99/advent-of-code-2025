(ns aoc25.d03
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))


(def in (mapv #(mapv atoi %) (get-line-chars)))

(def get-max
  (memoize
    (fn [n l]
      (if (= n 1)
        (apply max l)
        (let [first-range (- (count l) (dec n))
              mx (apply max (take first-range l))]
          (->> (range first-range)
               (filter #(= (nth l %) mx))
               (mapv #(get-max (dec n) (drop (inc %) l)))
               (apply max)
               (+ (->> n dec (Math/pow 10) long (* mx)))))))))

(defn q1 [bl]
  (->> bl
       (map #(get-max 2 %))
       (apply +)))


(defn q2 [bl]
  (->> bl
       (map #(get-max 12 %))
       (apply +)))

#_(q1 in)
#_(q2 in)
