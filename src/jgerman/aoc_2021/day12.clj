(ns jgerman.aoc-2021.day12
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn build-adjacency-map [ns]
  (reduce (fn [m [k v]]
            (-> m
                (update k (fn [old] (conj old v)))
                (update v (fn [old] (conj old k)))))
          {}
          ns))

(defn resource->input
  "We assume that the adjacency list in the input contains no duplicates."
  [resource]
  (->> resource
       utils/resource->lines
       (map #(str/split % #"-"))
       build-adjacency-map))

(defn big-cave? [n]
  (every? #(Character/isUpperCase %) n))

(defn exit-path? [ns]
  (let [[s e] ((juxt first last) ns)]
    (and (= "start" s)
         (= "end" e))))

(defn get-children [g current-path node]
  (let [children (get g node)]
    (filter (fn [n]
              (or (big-cave? n)
                  (not (some #{n} current-path)))) children)))

(defn find-paths
  ([g]
   (find-paths g [] [] "start"))
  ([g paths current-path node]
   #_(tap> {:node node
            :current-path current-path
            :paths paths})
   (let [children (get-children g current-path node)
         current-path (conj current-path node)]
     (cond
       (= node "end") (conj paths current-path)
       :else (mapcat (partial find-paths g paths current-path) children)))))

(defn task-1 [resource]
  (-> resource
      resource->input
      find-paths
      count))

(comment
  (def sample (resource->input "day12_sample1.txt"))
  (count (find-paths sample))
  (= 10 (task-1 "day12_sample1.txt"))
  (= 19 (task-1 "day12_sample2.txt"))
  (= 226 (task-1 "day12_sample3.txt"))
  (task-1 "day12_input.txt")
  ;;marker
  )
