(ns jgerman.aoc-2021.day9
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.core.matrix :as mat]
            [clojure.string :as str]))

(defn string-vec->int [v]
  (mapv #(Integer/parseInt %) v))

(defn resource->input [resource]
   (->> resource
        utils/resource->lines
        (mapv #(str/split % #""))
        (mapv string-vec->int)))

(defn raw-neighbors [y x]
  [[(dec y) x]
   [(inc y) x]
   [y (inc x)]
   [y (dec x)]])

(defn point->y [p] (first (:loc p)))
(defn point->x [p] (second (:loc p)))

(defn neighbor-cells [matrix y x]
  (let [[max-y max-x] (mat/shape matrix)]
    (vec (->> (raw-neighbors y x)
              (filter (fn [[y _]] (< y max-y)))
              (filter (fn [[_ x]] (< x max-x)))
              (filter (fn [vs]
                        (every? #(<= 0 %) vs)))))))

(defn neighbor-values [matrix y x]
  (map (fn [[y x]]
         (mat/mget matrix y x)) (neighbor-cells matrix y x)))

(defn find-low-points [matrix]
  (let [[max-y max-x] (mat/shape matrix)]
    (filter identity (for [y (range 0 max-y)
                           x (range 0 max-x)]
                       (let [val (mat/mget matrix y x)
                             neighbor-vals (neighbor-values matrix y x)]
                         (when (every? #(< val %) neighbor-vals)
                           {:loc [y x] :val val}))))))

(defn risk-level [matrix]
  (let [lps (map :val (find-low-points matrix))]
    (+ (apply + lps) (count lps))))

(defn flood-filter [matrix in-basin points]
  (filter (fn [p]
            (and (not= 9 (mat/mget matrix (first p) (second p)))
                 (not (contains? in-basin p))))
          points))

(defn find-basin [m p]
  (loop [in-basin #{(:loc p)}
         to-scan (flood-filter m in-basin (neighbor-cells m (point->y p) (point->x p)))]
    (if (nil? (first to-scan))
      (count in-basin)
      (recur (conj in-basin (first to-scan))
             (concat (rest to-scan)
                     (flood-filter m
                                   in-basin
                                   (neighbor-cells m
                                                   (first (first to-scan))
                                                   (second (first to-scan)))))))))
(defn task-1 []
  (-> "day9_input.txt"
      resource->input
      risk-level))

(defn task-2 []
  (let [m (-> "day9_input.txt" resource->input)
        lps (find-low-points m)]
    (apply * (take-last 3 (sort (map (partial find-basin m) lps))))))

(comment
  (require '[clojure.tools.deps.alpha.repl :refer [add-libs]])
  (add-libs
   '{net.mikera/core.matrix {:mvn/version "0.62.0"}})

  (def sample (resource->input "day9_sample.txt"))
  (find-basin sample (first (find-low-points sample)))
  (risk-level sample)
  (time (task-1))
  (= 562 (task-1))
  (task-2)

  ;; marker
  )
