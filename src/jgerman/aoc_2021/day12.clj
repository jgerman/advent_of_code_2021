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

(defn get-children [g current-path node]
  (let [children (get g node)]
    (filter (fn [n]
              (or (big-cave? n)
                  (not (some #{n} current-path)))) children)))

(defn find-paths
  ([g]
   (find-paths g [] [] "start"))
  ([g paths current-path node]
   (let [children (get-children g current-path node)
         current-path (conj current-path node)]
     (cond
       (= node "end") (conj paths current-path)
       :else (mapcat (partial find-paths g paths current-path) children)))))

(defn used-twice? [p]
  (not (every? #(= 1 %) (vals (frequencies (filter #(not (big-cave? %)) p))))))

(defn can-add? [current-path node]
  (when (= current-path ["start" "b" "d" "b" "A"])
    (tap> {:current-path current-path
           :node node
           :not-used-twice? (not (used-twice? current-path))
           :already-there? (some #{node} current-path)}))
  (cond
    (big-cave? node) true
    (and (= node "start")
         (empty? current-path)) true ;; I'm sure I could do this better
    (= node "start") false
    (= node "end") true ;; this is always ignored actually just makes path-search easier for now
    (not (used-twice? current-path)) true
    (some #{node} current-path) false
    :else true))

(defn path-search
  ([g]
   (path-search g [] [] "start"))
  ([g paths current-path node]
   (let [add? (can-add? current-path node)
         children (get g node)]
     (when (= current-path ["start" "b" "d" "b" "A"])
       (tap> {:paths paths
              :current-path current-path
              :node node
              :add? add?}))
     (cond
       (= node "end") (conj paths (conj current-path node)) ;; not striclty necessary to add the end
       (not add?) paths
       :else (mapcat (partial path-search g paths (conj current-path node)) children)))))

(defn task-1 [resource]
  (-> resource
      resource->input
      find-paths
      count))

(defn task-2 [resource]
  (-> resource
      resource->input
      path-search))

(comment
  (def sample (resource->input "day12_sample1.txt"))

  (= 10 (task-1 "day12_sample1.txt"))
  (= 19 (task-1 "day12_sample2.txt"))
  (= 226 (task-1 "day12_sample3.txt"))
  (= 4775 (task-1 "day12_input.txt"))


  (= 36 (count (task-2 "day12_sample1.txt")))
  (= 103 (count (task-2 "day12_sample2.txt")))
  (= 3509 (count (task-2 "day12_sample3.txt")))
  (= 152480 (count (task-2 "day12_input.txt")))
  ;;marker
  )
