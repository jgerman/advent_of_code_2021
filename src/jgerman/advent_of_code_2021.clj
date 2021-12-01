(ns jgerman.advent-of-code-2021
  (:require [clojure.java.io :as io]))

(defn resource->edn [resource]
  (-> resource
      io/resource
      slurp
      read-string))

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

(comment
  (def sample-depths [199 200 208 210 200 207 240 269 260 263])
  (count-increases (map-changes sample-depths))
  (count-increases (map-changes (resource->edn "day1_input1.edn")))
  (map-sliding-window sample-depths 3)
  (count-increases (map-changes (map-sliding-window sample-depths 3)))
  (-> "day1_input1.edn"
      resource->edn
      map-changes
      count-increases)
  (-> "day1_input1.edn"
      resource->edn
      (map-sliding-window 3)
      map-changes
      count-increases)
  ,)
