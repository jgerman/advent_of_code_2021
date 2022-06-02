(ns jgerman.aoc-2021.day23
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]
            [clojure.core.matrix :as m]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(defn process-line [s]
  (-> s
      (str/replace #"\s" "#")
      (str/split #"")))

(defn resource->input [resource]
  (let [cave (->> resource
                  utils/resource->lines
                  (map process-line))]
    {:cave cave
     :energy 0}))

(defn open-spaces [cave]
  (filter (fn [[row col]]
            (= "." (m/mget cave row col))) (m/index-seq cave)))

(defn navigable-spaces [cave]
  (filter (fn [[row col]]
            (not= "#" (m/mget cave row col))) (m/index-seq cave)))

(defn neighbors [[row col]]
  (tap> {:getting neighbors
         :row row
         :col col})
  [[(inc row) col]
   [(dec row) col]
   [row (inc col)]
   [row (dec col)]])

(defn build-adj [cave]
  (let [spaces (navigable-spaces cave)]
    (loop [acc {}
           sps spaces]
      (tap> {:checking (first sps)})
      (if (empty? sps)
        acc
        (let [nbs (filter (fn [n]
                                  (some #{n} spaces))
                          (neighbors (first sps)))]
          (recur (assoc acc (first sps) nbs)
                 (rest sps)))))))



(defn path [pathing to from]
  (rest (alg/bf-path pathing to from)))

(defn hallway?
  ([loc] (apply hallway? loc))
  ([row _]
   (= row 1)))

(defn amber-room?
  ([loc] (apply amber-room? loc))
  ([row col]
   (and (= col 3)
        (< 1 row))))

(defn available-room? [cave filter-fn v]
  (let [spaces (filter filter-fn (m/index-seq cave))
        empty-amber (filter #(some #{%} (open-spaces cave)))]
    (when (every? (fn [[r c]]
                    (or (= v (m/mget cave r c))
                        (= "." (m/mget cave r c)))) spaces)
      (first (reverse (sort empty-amber))))))

(defn available-amber? [cave]
  (available-room? cave amber-room? "A"))

(defn bronze-room?
  ([loc] (apply bronze-room? loc))
  ([row col]
   (and (= col 5)
        (< 1 row))))

(defn available-bronze? [cave]
  (available-room? cave bronze-room? "B"))

(defn copper-room?
  ([loc] (apply copper-room? loc))
  ([row col]
   (and (= col 7)
        (< 1 row))))

(defn available-copper? [cave]
  (available-room? cave copper-room? "C"))

(defn desert-room?
  ([loc] (apply desert-room? loc))
  ([row col]
   (and (= col 9)
        (< 1 row))))

(defn available-desert? [cave]
  (available-room? cave desert-room? "D"))

(defn hallway-block?
  ([loc] (apply hallway-block? loc))
  ([row col]
   (and (hallway? row col)
        (some #{col} [3 5 7 9]))))

(defmulti valid-moves' (fn [_ v _ _] v))

(defmethod valid-moves' "A"
  [cave _ row col]
  (let [open-spaces (open-spaces cave)]
    (cond
      (hallway? row col) (when-let [valid-space (filter amber-room? open-spaces)]
                           )

      )))

(defn valid-moves [cave row col]
  (let [v (m/mget cave row col)]
    (valid-moves' cave v row col)))

(comment
  (def sample (resource->input "day23_sample.txt"))
  (def pathing (graph/graph (build-adj (:cave sample))))


  ;; marker

  ;; task 1 13445
  ;; 53767
  )
