(ns aoc25.d07
  (:require [clojure.string :as s]
            [aoc25.utils :refer :all]))

(def in (->> (get-line-chars) (take-nth 2) vec))

(defn q1
  [[line & lines]]
  (loop [beams #{(->> (count line) range
                    (some #(when (= (get line %) \S) %)))}
         ret 0
         lines lines]
    (if (empty? lines)
      ret
      (let [line (first lines)
            nbeams (->> beams
                        (mapcat (fn [beam]
                                  (if (= (get line beam) \^)
                                    [(dec beam) (inc beam)]
                                    [beam])))
                        set)
            cnt (count (filter #(= (get line %) \^) beams))]
        (recur nbeams (+ ret cnt) (rest lines))))))

#_(q1 in)

(defn q2
  [[line & lines]]
  (loop [beams [[(->> (count line) range
                      (some #(when (= (get line %) \S) %)))
                 1]]
         lines lines]
    (if (empty? lines)
      (apply + (map second beams))
      (let [[line & lines] lines
            beams (->> beams
                       (mapcat (fn [[pt tcnt]]
                                 (if (= (get line pt) \^)
                                   [[(dec pt) tcnt]
                                    [(inc pt) tcnt]]
                                   [[pt tcnt]])))
                       (group-by first)
                       (map (fn [[pt tbeams]]
                              [pt (apply + (map second tbeams))]))
                       )]
        (recur beams lines)))))

#_(q2 in)
