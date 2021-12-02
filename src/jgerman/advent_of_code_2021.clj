(ns jgerman.advent-of-code-2021
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn resource->edn [resource]
  (-> resource
      io/resource
      slurp
      read-string))

(defn resource->lines [resource]
  (-> resource
      io/resource
      slurp
      str/split-lines))

(defn resource->day2-input [resource]
  (map (fn [line]
         (let [[direction magnitude] (str/split line #" ")]
           [(keyword direction) (Integer/parseInt magnitude)]))
       (resource->lines resource)))

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

(defn apply-instruction [{:keys [aim] :as location}
                         [direction magnitude]]
  (case direction
    :forward (-> location
                 (update :horizontal + magnitude)
                 (update :vertical + (* aim magnitude)))
    :down (update location :aim + magnitude)
    :up (update location :aim - magnitude)))

(defn pilot [directions]
  (reduce (fn [location instruction]
            (apply-instruction location instruction))
          {:horizontal 0
           :vertical 0
           :aim 0}
          directions))

(defn location->day2-solution [{:keys [horizontal vertical]}]
  (* horizontal vertical))

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
  (resource->day2-input "day_2_input.txt")
  (apply-instruction {:horizontal 0 :vertical 0} [:up 10])
  (pilot (resource->day2-input "day_2_input.txt"))
  (pilot (resource->day2-input "day_2_sample.txt"))
  (-> "day_2_input.txt"
      resource->day2-input
      pilot
      location->day2-solution)
  ,)
