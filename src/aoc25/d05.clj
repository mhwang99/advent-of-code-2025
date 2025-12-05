(ns aoc25.d05
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(defn parse-input [res]
  (let [ll (get-line-num res)
        rgs (take-while #(> (count %) 0) ll)
        ids (drop (inc (count rgs)) ll)]
    {:ranges (mapv (fn [[s e]] [s (- e)]) rgs)
     :ids (mapcat identity ids)}))

(def in (parse-input (get-res)))

(defn q1
  [{:keys [ranges ids]}]
  (reduce
    (fn [ret id]
      (if (some (fn [[s e]] (<= s id e)) ranges)
        (inc ret)
        ret))
    0 ids))

#_(q1 in)

(defn linked?
  [[as ae] [bs be]]
  (and (<= as be) (<= bs ae)))

(defn link
  [[as ae] [bs be]]
  [(min as bs) (max ae be)])

(defn link-list
  [ll]
  (loop [ll (sort ll)]
    (let [new-ll (reduce
                   (fn [ll l]
                     (if (and (seq ll)
                              (linked? (first ll) l))
                       (conj (rest ll)
                             (link (first ll) l))
                       (conj ll l)))
                   '() ll)]
      (if (= (count ll) (count new-ll))
        ll
        (recur new-ll)))))

(defn q2
  [{:keys [ranges]}]
  (->> (link-list ranges)
       (map (fn [[s e]] (inc (- e s))))
       (apply +)))

#_(q2 in)
