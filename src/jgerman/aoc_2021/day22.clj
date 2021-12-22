(ns jgerman.aoc-2021.day22
  (:require
   [jgerman.aoc-2021.utils :as utils]
   [clojure.string :as str]))

(defrecord cuboid [cmd x1 x2 y1 y2 z1 z2])

(defn parse-line [l]
  (let [line (-> l
                 (str/replace #"x=" "")
                 (str/replace #"y=" "")
                 (str/replace #"z=" "")
                 (str/replace #"," " ")
                 (str/replace #"\.\." " ")
                 (str/split #"\s+"))]
    (apply ->cuboid (conj (map #(Integer/parseInt %) (rest line))
                          (keyword (first line))))))
(defn resource->input [resource]
  (->> resource
       utils/resource->lines
       (map parse-line)))

(def t1-between #(<= -50 % 50))

(defn in-bounds? [c]
  (every? identity
          (map t1-between ((juxt :x1 :x2 :y1 :y2 :z1 :z2) c))))

(defn cuboid->range [c dim1 dim2]
  (if (< (dim1 c) (dim2 c))
    (range (dim1 c) (inc (dim2 c)))
    (range (dim2 c) (inc (dim1 c)))))

;; ugly but does the job... must be a cleaner way
(defn cuboid->points [cuboid]
  (partition 3
             (flatten (for [x (cuboid->range cuboid :x1 :x2)]
                        (for [y (cuboid->range cuboid :y1 :y2)]
                          (for [z (cuboid->range cuboid :z1 :z2)]
                            [x y z]))))))

(defn process-cuboid [cubes-on cuboid]
  (let [process-fn (if (= :on (:cmd cuboid)) conj disj)]
    (reduce (fn [acc pt]
              (process-fn acc pt))
            cubes-on
            (cuboid->points cuboid))))

(defn do-initialization-procedure [cuboids]
  (reduce (fn [acc c]
            (if (not (in-bounds? c))
              acc
              (process-cuboid acc c)))
          #{}
          cuboids))

(defn do-full-procedure [cuboids]
  (reduce (fn [acc c]
            (process-cuboid acc c))
          #{}
          cuboids))

(defn task-1 [resource]
  (-> resource
      resource->input
      do-initialization-procedure
      count))

(defn task-2 [resource]
  (-> resource
      resource->input
      do-full-procedure
      count))





(comment
  (def sample (resource->input "day22_sample.txt"))
  (= 553201 (task-1 "day22_input.txt"))
  (task-2 "day22_sample2.txt")
  ;;marker
  )
