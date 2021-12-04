(ns jgerman.aoc-2021.day1
  (:require [jgerman.aoc-2021.utils :as utils]))

(defn map-changes
  "Given a collection of depths in integers return a collection containing true
  for the pairs that increase and false otherwise."
  [depths]
  (map (fn [x y]
         (< x y)) depths (rest depths)))

(defn map-sliding-window [depths window-size]
  (loop [window (take window-size depths)
         remaining (drop 1 depths)
         acc []]
    (if (< (count window) window-size)
      acc
      (recur (take window-size remaining)
             (drop 1 remaining)
             (conj acc (apply + window))))))

(defn count-increases [depth-changes]
  (count (filter identity depth-changes)))

(defn day1-task1 []
  (-> "day1_input1.edn"
      utils/resource->edn
      map-changes
      count-increases))

(defn day1-task2 []
  (-> "day1_input1.edn"
      utils/resource->edn
      (map-sliding-window 3)
      map-changes
      count-increases))


(comment
  (= 1400 (day1-task1))
  (= 1429 (day1-task2))
  ,)
