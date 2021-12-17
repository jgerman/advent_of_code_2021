(ns jgerman.aoc-2021.day15
  (:require [jgerman.aoc-2021.utils :as utils]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [clojure.string :as str]
            [clojure.core.matrix :as mat]))

(defn string-vec->int [v]
  (mapv #(Integer/parseInt %) v))

(defn resource->input [resource]
   (->> resource
        utils/resource->lines
        (mapv #(str/split % #""))
        (mapv string-vec->int)))

(defn raw-neighbors [row col]
  [[row (dec col)]
   [row (inc col)]
   [(inc row) col]
   [(dec row) col]])

(defn neighbor-cells [matrix row col]
  (let [[max-row max-col] (mat/shape matrix)]
    (vec (->> (raw-neighbors row col)
              (filter (fn [[_ col]] (< col max-col)))
              (filter (fn [[row _]] (< row max-row)))
              (filter (fn [vs]
                        (every? #(<= 0 %) vs)))))))

(defn point-kw [row col] (keyword (str row "-" col)))

(defn build-adj [g row col]
  (let [nbs (neighbor-cells g row col)]
    (reduce (fn [acc [row col]]
              (assoc acc (point-kw row col) (mat/mget g row col))) ;; mixing coord systems here, be careful
            {}
            nbs)))

(defn input->adjacency-map [input]
  (let [g-seq (mat/index-seq input)]
    (reduce (fn [acc [row col]]
              (assoc acc (point-kw row col) (build-adj input row col)))
            {}
            g-seq)))

(defn inc-floor [g n]
  (mat/emap (fn [v]
              (let [tot (+ v n)]
                (if (> tot 9)
                  (mod tot 9)
                  tot))) g))

(defn large-row [start-g]
  (mapv (fn [row]
         (vec (concat row
                      (inc-floor row 1)
                      (inc-floor row 2)
                      (inc-floor row 3)
                      (inc-floor row 4))))
       start-g))

(defn build-large [g]
  (concat (large-row g)
          (large-row (inc-floor g 1))
          (large-row (inc-floor g 2))
          (large-row (inc-floor g 3))
          (large-row (inc-floor g 4))))

(defn task-1 [resource]
  (let [input (resource->input resource)
        adjacency (input->adjacency-map input)
        g (graph/weighted-digraph adjacency)
        bounds (mat/shape input)]
    (second (alg/dijkstra-path-dist g :0-0 (point-kw (dec (first bounds)) (dec (second bounds)))))))

(defn task-2 [resource]
  (let [input (build-large (resource->input resource))
        adjacency (input->adjacency-map input)
        g (graph/weighted-digraph adjacency)
        bounds (mat/shape input)]
    (second (alg/dijkstra-path-dist g :0-0 (point-kw (dec (first bounds)) (dec (second bounds)))))))

(comment
  (= 720 (task-1 "day15_input.txt"))
  (= 315 (task-2 "day15_sample.txt"))
  (= 3025 (task-2 "day15_input.txt"))
  ;;marker
  ,)

;;  == 720, but I'm getting 712...
