(ns jgerman.aoc-2021.day11
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
   [(inc y) x] [y (inc x)] [y (dec x)]
   [(inc y) (inc x)] [(dec y) (dec x)] [(dec y) (inc x)] [(inc y) (dec x)]])

(defn neighbor-cells [matrix y x]
  (let [[max-y max-x] (mat/shape matrix)]
    (vec (->> (raw-neighbors y x)
              (filter (fn [[y _]] (< y max-y)))
              (filter (fn [[_ x]] (< x max-x)))
              (filter (fn [vs]
                        (every? #(<= 0 %) vs)))))))

(defn find-indices [f m]
  (filter (fn [[y x]]
            (f (mat/mget m y x))) (mat/index-seq m)))

(defn m-update [m [y x] f]
  (mat/mset m y x (f (mat/mget m y x))))

(defn power-up [m [y x]]
  (let [neighbors (neighbor-cells m y x)]
    (reduce (fn [acc idx]
              (m-update acc idx (fn [x] (if (= 0 x) x (inc x)))))
            m
            neighbors)))

(defn reset [m [y x]]
  (mat/mset m y x 0))

(defn flash [m]
  (let [flashers (find-indices #(< 9 %) m)
        flashed (reduce reset m flashers)]
    (reduce power-up flashed flashers)))

(defn process-flashes [m]
  (loop [acc m
         total-flashes 0]
    (let [flashed-m (flash acc)
          flasher-count (mat/zero-count flashed-m)]
      (if (= total-flashes flasher-count)
        {:map acc :flashes flasher-count}
        (recur flashed-m flasher-count)))))

(defn step [m]
  (->> m
       (mat/emap inc)
       process-flashes))

(defn do-steps [matrix steps]
  (loop [m matrix
         flashes 0
         idx 0]
    (if (= idx steps) flashes
        (let [res (step m)]
          (recur (:map res) (+ flashes (:flashes res)) (inc idx))))))

(defn synced? [m]
  (= (mat/zero-count m) (apply * (mat/shape m))))

(defn find-sync [matrix]
  (loop [m matrix
         idx 0]
    (if (synced? m)
      idx
      (let [res (step m)]
        (recur (:map res) (inc idx))))))

(defn task-1 []
  (-> "day11_input.txt"
      resource->input
      (do-steps 100)))

(defn task-2 []
  (-> "day11_input.txt"
      resource->input
      find-sync))

(comment
  (= 1694 (task-1))
  (= 346 (task-2))
  ;; marker
  )
