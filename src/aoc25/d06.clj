(ns aoc25.d06
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(defn q1
  [s]
  (->> (s/split s #"\n")
       (mapv #(re-seq #"[0-9+*]+" %))
       (apply
         map
         (fn [& l]
           (apply (if (= "*" (last l)) * +)
                  (map atoi (drop-last l)))))
       (apply +)))

#_(q1 (get-res))

(defn q2
  [s]
  (->> (s/split s #"\n")
       (mapv reverse)
       (apply map vector)
       (reduce
         (fn [[ret pl] l]
           (let [n (s/trim (apply str (drop-last l)))
                 e (s/trim (str (last l)))]
             (cond
               (empty? n) [ret []]
               (empty? e) [ret (conj pl (atoi n))]
               :else (let [op (if (= e "*") * +)
                           pl (conj pl (atoi n))]
                       [(+ ret (apply op pl)) []]))))
         [0 []])))

#_(q2 (get-res))
