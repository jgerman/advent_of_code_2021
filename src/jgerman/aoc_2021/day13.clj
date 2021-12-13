(ns jgerman.aoc-2021.day13
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn parts->input [[coords folds]]
  {:paper (map #(str/split % #",") (-> coords (str/split #"\n")))
   :folds (map (fn [s]
                 (-> s
                     (str/replace "fold along " "")
                     (str/split #"="))) (str/split folds #"\n"))})

(defn string->int [s] (Integer/parseInt s))

(defn input->ints [{:keys [paper folds]}]
  {:paper (map (fn [[x y]] [(string->int x) (string->int y)]) paper)
   :folds (map (fn [[a b]] [a (string->int b)]) folds)})

(defn resource->input [resource]
  (-> resource
      utils/resource->text
      (str/split #"\n\n")
      parts->input
      input->ints))


(defn fold-x [paper coord]
  (map (fn [[x y]]
         [(if (< x coord) (+ coord
                             (- coord x)) x)
          y])
       paper))

(defn fold-y [paper coord]
  (map (fn [[x y]]
         [x
          (if (< y coord) (+ coord
                             (- coord y)) y)])
       paper))

(defn fold [paper [fold-axis coord]]
  (tap> {:fold-axis fold-axis
         :coord coord})
  (if (= "x" fold-axis)
    (distinct (fold-x paper coord))
    (distinct (fold-y paper coord))))

(defn fold-it [{:keys [paper folds]}]
  (distinct (reduce (fn [p f]
                      (fold p f)) paper folds)))

(defn limit-folds [input n]
  (update input :folds (partial take n)))

(defn task-1 [resource n]
  (-> resource
      resource->input
      (limit-folds n)
      fold-it))

(defn max-x [pts] (apply max (map first pts)))
(defn max-y [pts] (apply max (map second pts)))

(defn plot-it [paper]
  true)

(defn task-2 [resource]
  (-> resource
      resource->input
      fold-it
      plot-it))


(comment
  (resource->input "day13_sample.txt")
  (map inc #{1 2})

  (= 17 (count (task-1 "day13_sample.txt" 1)))

  (count (task-1 "day13_input.txt" 1))
  (task-2 "day13_input.txt")
  ;;marker
  )
