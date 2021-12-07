(ns jgerman.aoc-2021.day7
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn resource->input [resource]
  (map #(Integer/parseInt %)
       (-> resource
           utils/resource->text
           str/trim
           (str/split #","))))

(defn simple-fuel-cost [loc target]
  (Math/abs (- loc target)))

(defn complex-fuel-cost [loc target]
  (let [distance (simple-fuel-cost loc target)]
    (apply + (range 0 (inc distance)))))

(defn crabs-to-location-cost [crabs loc cost-fn]
  (reduce + (map (fn [c]
                   (cost-fn c loc)) crabs)))

(defn find-costs [crabs cost-fn]
  (let [min-loc (apply min crabs)
        max-loc (apply max crabs)]
    (for [idx (range min-loc (inc max-loc))]
      [idx (crabs-to-location-cost crabs idx cost-fn)])))

(defn min-cost [costs]
  ((comp second first)
   (sort (fn [[_ cost1] [_ cost2]]
           (< cost1 cost2)) costs)))

(defn task-1 []
  (-> "day7_input.txt"
      resource->input
      (find-costs simple-fuel-cost)
      min-cost))

(defn task-2 []
  (-> "day7_input.txt"
      resource->input
      (find-costs complex-fuel-cost)
      min-cost))

(comment
  (def sample (resource->input "day7_sample.txt"))
  (complex-fuel-cost 16 5)
  (= 336721 (task-1))
  (task-2)
  ;;marker
  )
