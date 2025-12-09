(ns aoc25.d09
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(def in (get-line-num))

(defn get-area
  [[ax ay] [bx by]]
  (* (inc (Math/abs (- ax bx)))
     (inc (Math/abs (- ay by)))))

(defn q1
  [pts]
  (loop [[pt & pts] pts
         mx 0]
    (if (empty? pts)
      mx
      (let [areas (map #(get-area pt %) pts)]
        (recur pts (apply max mx areas))))))

(defn get-all-lines
  [pts]
  (->> (partition 2 1 pts pts)
       (mapv (fn [[[ax ay] [bx by]]]
               [(min ax bx) (min ay by)
                (max ax bx) (max ay by)]))))

(defn cross-lines?
  [[ax ay] [bx by] lines]
  (let [[ax bx] (sort [ax bx])
        [ay by] (sort [ay by])]
    (some
      (fn [[cx cy dx dy]]
        (and (and (< ax dx) (< cx bx))
             (and (< ay dy) (< cy by))))
      lines)))

(defn q2
  [pts]
  (let [lines (get-all-lines pts)]
    (loop [[apt & pts] pts
           mx 0]
      (if (empty? pts)
        mx
        (recur
          pts
          (reduce
            (fn [mx bpt]
              (let [area (get-area apt bpt)]
                (if (or (< area mx)
                        (cross-lines? apt bpt lines))
                  mx area)))
            mx pts))))))


#_(q1 in)
#_(q2 in)
