(ns jgerman.aoc-2021.day2
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn resource->day2-input [resource]
  (map (fn [line]
         (let [[direction magnitude] (str/split line #" ")]
           [(keyword direction) (Integer/parseInt magnitude)]))
       (utils/resource->lines resource)))

(defn apply-instruction-v1 [location
                            [direction magnitude]]
  (case direction
    :forward (update location :horizontal + magnitude)
    :down (update location :vertical + magnitude)
    :up (update location :vertical - magnitude)))

(defn apply-instruction [{:keys [aim] :as location}
                         [direction magnitude]]
  (case direction
    :forward (-> location
                 (update :horizontal + magnitude)
                 (update :vertical + (* aim magnitude)))
    :down (update location :aim + magnitude)
    :up (update location :aim - magnitude)))

(defn pilot-v1 [directions]
  (reduce (fn [location instruction]
            (apply-instruction-v1 location instruction))
          {:horizontal 0
           :vertical 0
           :aim 0}
          directions))

(defn pilot [directions]
  (reduce (fn [location instruction]
            (apply-instruction location instruction))
          {:horizontal 0
           :vertical 0
           :aim 0}
          directions))

(defn location->day2-solution [{:keys [horizontal vertical]}]
  (* horizontal vertical))

(defn day2-task1 []
  (-> "day_2_input.txt"
      resource->day2-input
      pilot-v1
      location->day2-solution))

(defn day2-task2 []
  (-> "day_2_input.txt"
      resource->day2-input
      pilot
      location->day2-solution))


(comment
  (= 1459206 (day2-task1))
  (= 1320534480 (day2-task2))
  ,)
