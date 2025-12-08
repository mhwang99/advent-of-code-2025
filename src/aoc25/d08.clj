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
  (loop [pts (sort pts)
         ret []]
    (if (< (count pts) 2)
      (sort ret)
      (let [[pt & npts] pts]
        (recur npts
               (into ret
                     (map (fn [apt bpt]
                            [(get-distance apt bpt) apt bpt])
                          npts
                          (repeat pt))))))))

(defn merge-group
  [groups apt bpt]
  (loop [ret []
         ngrp #{}
         groups groups]
    (if (empty? groups)
      (conj ret ngrp)
      (let [[grp & groups] groups]
        (if (or (grp apt) (grp bpt))
          (recur ret (into ngrp grp) groups)
          (recur (conj ret grp) ngrp groups))))))

(defn q1
  [pts n]
  (loop [n n
         diss (get-all-distances pts)
         groups (mapv (fn [pt] #{pt}) pts)]
    (if (= n 0)
      (->> groups (map count) sort reverse (take 3) (apply *))
      (let [[dis & ndiss] diss
            [_ apt bpt] dis
            ngroups (merge-group groups apt bpt)]
        (recur (dec n)
               ndiss
               ngroups)))))

(defn q2
  [pts]
  (loop [diss (get-all-distances pts)
         groups (mapv (fn [pt] #{pt}) pts)]
    (let [[dis & ndiss] diss
          [_ apt bpt] dis
          ngroups (merge-group groups apt bpt)]
      (if (= 1 (count ngroups))
        (* (first apt) (first bpt))
        (recur ndiss
               ngroups)))))

#_(q1 in 1000)
#_(q2 in)
