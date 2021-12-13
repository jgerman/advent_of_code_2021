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

(defn small-cave-limit? [p limit]
  (not (every? #(< % limit) (vals (frequencies (filter #(not (big-cave? %)) p))))))

(defn can-add? [current-path small-limit node]
  (cond
    (big-cave? node) true
    (and (= node "start")
         (empty? current-path)) true ;; I'm sure I could do this better
    (= node "start") false
    (not (small-cave-limit? current-path small-limit)) true
    (some #{node} current-path) false
    :else true))

(defn path-search
  ([g small-limit]
   (path-search g small-limit nil [] "start"))
  ([g small-limit paths current-path node]
   (let [add? (can-add? current-path small-limit node)
         children (get g node)]
     (cond
       (= node "end") (conj paths (conj current-path node)) ;; not striclty necessary to add the end
       (not add?) paths
       :else (mapcat (partial path-search g small-limit paths (conj current-path node)) children)))))

(defn task-1 [resource]
  (-> resource
      resource->input
      (path-search 1)))

(defn task-2 [resource]
  (-> resource
      resource->input
      (path-search 2)))

(comment
  (= 10 (count (task-1 "day12_sample1.txt")))
  (= 19 (count (task-1 "day12_sample2.txt")))
  (= 226 (count (task-1 "day12_sample3.txt")))
  (= 4775 (count (task-1 "day12_input.txt")))

  (= 36 (count (task-2 "day12_sample1.txt")))
  (= 103 (count (task-2 "day12_sample2.txt")))
  (= 3509 (count (task-2 "day12_sample3.txt")))
  (= 152480 (count (task-2 "day12_input.txt")))
  ;;marker
  )
