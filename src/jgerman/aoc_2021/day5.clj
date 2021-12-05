(ns jgerman.aoc-2021.day5
  (:require [clojure.string :as str]
            [jgerman.aoc-2021.utils :as utils]))

;; still flirting with writing a parser for this but it's still straighforward
;; enough to do splits
(defn string->coords [s]
  (map #(Integer/parseInt %)
   (str/split s #",")))

(defn parse-line [l]
  (let [parts (str/split (str/trim l) #"\s+")]
    [(string->coords (nth parts 0))
     (string->coords (nth parts 2))]))


(defn resource->input [resource]
  (let [lines (-> resource
                  utils/resource->lines)]
    (mapv parse-line lines)))

;; for task 1 we're only considering orthoganol line segments so we can
;; write a pretty naive implementation
(defn vertical-line? [[[x1 _] [x2 _]]]
  (= 0 (- x1 x2)))

(defn horizontal-line? [[[_ y1] [_ y2]]]
  (= 0 (- y1 y2)))

(defn orthoganol? [line-segment]
  (or (vertical-line? line-segment)
      (horizontal-line? line-segment)))

(defn only-orthoganol [line-segments]
  (filter orthoganol? line-segments))

;; quick and dirty
(defn vertical-sort
  "ys change, xs do not."
  [point-pair]
  (sort (fn [[_ y1] [_ y2]]
          (< y1 y2))
        point-pair))

(defn horizontal-sort
  "xs change, ys do not."
  [point-pair]
  (sort (fn [[x1 _] [x2 _]]
          (< x1 x2))
        point-pair))

(defn vertical-line [line-segment]
  (let [sorted-segment (vertical-sort line-segment)
        [[x y1] [_ y2]] sorted-segment]
    (for [y (range y1 (inc y2))]
      [x y])))

(defn horizontal-line [line-segment]
  (let [sorted-segment (horizontal-sort line-segment)
        [[x1 y] [x2 _]] sorted-segment]
    (for [x (range x1 (inc x2))]
      [x y])))

;; diagonal lines are only 45 degrees, which means for every x we go up 1 we
;; also go up 1 for y
;;
;; good thing I didn't wast time implementing the general purpose
;; algorithm (though I should for the future)
;;
;; we can sort by x, modify both until the target is reached then return the list
(defn get-y-direction [[[_ y1] [_ y2]]]
  (if (< y1 y2) inc dec))

(defn diagonal-line [line-segment]
  (let [sorted-segment (horizontal-sort line-segment)
        y-direction (get-y-direction sorted-segment)
        [[x1 y1] [x2 y2]] sorted-segment]
    (loop [x x1
           y y1
           points []]
      (if (= [x y] [x2 y2])
        (conj points [x y])
        (recur (inc x) (y-direction y) (conj points [x y]))))))

(defn line-segment->points [line-segment]
  (cond
    (horizontal-line? line-segment) (horizontal-line line-segment)
    (vertical-line? line-segment) (vertical-line line-segment)
    :else (diagonal-line line-segment)))

(defn line-segments->points [segments]
  (mapcat line-segment->points segments))

(defn frequencies->danger [frequencies]
  (filter (fn [[_ v]]
            (< 1 v))
          frequencies))

(defn task-1 []
  (-> "day5_input.txt"
      resource->input
      only-orthoganol
      line-segments->points
      frequencies
      frequencies->danger
      keys
      count))

(defn task-2 []
  (-> "day5_input.txt"
      resource->input
      line-segments->points
      frequencies
      frequencies->danger
      keys
      count))


(comment
  (def sample-input (resource->input "day5_sample.txt"))
  (orthoganol? (nth sample-input 1))
  (only-orthoganol sample-input)
  (vertical-sort [[0 15] [0 12]])
  (horizontal-sort [[12 0] [15 0]])
  (vertical-line [[0 15] [0 12]])
  (horizontal-line [[12 0] [15 0]])
  (apply = ((juxt line-segment->points
                  horizontal-line) [[12 0] [15 0]]))
  (apply = ((juxt line-segment->points
                  vertical-line) [[12 0] [15 0]]))
  (apply = ((juxt line-segment->points
                  vertical-line) [[0 15] [0 12]]))
  (line-segments->points (only-orthoganol sample-input))
  (diagonal-line [[1 1] [3 3]])
  (diagonal-line [[9 7] [7 9]])

;; tests
  (= 8350 (task-1))
  (= 19374 (task-2))

;; marker
  )
