(ns jgerman.aoc-2021.day17
  (:require [jgerman.aoc-2021.utils :as utils]))

;; no point in parsing the original input, it's so small I just convert it to
;; edn by hand
(defn resource->input [resource]
  (-> resource
      utils/resource->edn))

(defn triangle-num [n]
  (/ (* n (inc n)) 2))

(defn find-lowest-x-vel [target]
  (loop [n 0] (if (>= (triangle-num n) target) n (recur (inc n)))))

(defn drag [x]
  (if (<= x 1) 0 (dec x)))

(defn step [x y xv yv]
  [(+ x xv)
   (+ y yv)
   (drag xv)
   (- yv 1)])

(defn between [n n1 n2]
  (<= n1 n n2))

(defn valid-path? [points {:keys [min-x max-x min-y max-y]}]
  (some (fn [[x y]]
          (and (between x min-x max-x)
               (between y min-y max-y)))
        points))

(defn sim [xv yv max-x min-y]
  (loop [points []
         x 0
         y 0
         xv xv
         yv yv]
    (if (or (< max-x x)
            (> min-y y))
      points
      (let [[new-x new-y new-xv new-yv] (step x y xv yv)]
        (recur (conj points [x y]) new-x new-y new-xv new-yv)))))

(defn path->max-height [pts] (apply max (map second pts)))

(def max-yv 500) ;; this is probably calculable... picking a num to solve the problem for now
(def min-yv -500)

(defn paths-for-xv [xv {:keys [max-x min-y] :as bounds}]
  (for [yv (range min-yv max-yv)]
    (sim xv yv max-x min-y)))

(defn safe-max [xs] (if (empty? xs) -1 (apply max xs)))

(defn valid-paths-for-xv [xv bounds]
  (->> (paths-for-xv xv bounds)
       (filter #(valid-path? % bounds))))

(defn max-height-for-xv [xv bounds]
  (->> (valid-paths-for-xv xv bounds)
       (map path->max-height)
       safe-max))

;; I can quickly get part 2 just by counting the values thare aren't -1 because
;; that means they had a valid max height
(defn calc-valid-initial-velocities [{:keys [max-x min-x max-y min-y] :as bounds}]
  (let [max-forward max-x
        min-forward (find-lowest-x-vel min-x)]
    (loop [acc []
           xv min-forward]
      (if (< max-forward xv)
        acc
        (recur (concat acc (valid-paths-for-xv xv bounds)) (inc xv))))))

(defn calc-max-height [{:keys [max-x min-x max-y min-y] :as bounds}]
  (let [max-forward max-x
        min-forward (find-lowest-x-vel min-x)]
    (apply max (for [xv (range min-forward (inc max-forward))]
                 (max-height-for-xv xv bounds)))))

(defn task-1 [resource]
  (-> resource
      resource->input
      calc-max-height))

(defn task-2 [resource]
  (-> resource
      resource->input
      calc-valid-initial-velocities
      count))

(comment
  (calc-max-height sample-bounds)
  (= 12561 (task-1 "day17_input.edn"))
  (= 3785 (task-2 "day17_input.edn"))

  ;;marker
  )
