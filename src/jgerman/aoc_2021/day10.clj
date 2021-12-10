(ns jgerman.aoc-2021.day10
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn resource->input [resource]
  (map #(str/split % #"") (-> resource
                              utils/resource->lines)))

(def close->open {")" "(" "]" "[" "}" "{" ">" "<"})
(def open->close (set/map-invert close->open))

(def score {")" 3 "]" 57 "}" 1197 ">" 25137})
(def ac-score {")" 1 "]" 2 "}" 3 ">" 4})

(def open (vals close->open))
(def closed (keys close->open))
(defn open? [s] (some #{s} open))
(defn closed? [s] (some #{s} closed))

(defn parse-line [nil-fn mismatch line]
  (loop [stack '() chars line]
    (let [c (first chars)]
      (cond
        (nil? c) (nil-fn c line)
        (closed? c) (let [top (first stack)
                          match (get close->open c)]
                      (if (not= match top)
                        (mismatch c)
                        (recur (rest stack) (rest chars))))
        (open? c) (recur (conj stack c) (rest chars))))))

(defn scores [xs] (map #(get score %) xs))

(defn auto-complete [line]
  (loop [ls (reverse line)
         stack '()
         completion []]
    (let [c (first ls)]
      (cond
        (nil? c) completion
        (closed? c) (recur (rest ls) (conj stack c) completion)
        (open? c) (let [top (first stack)
                        match (get open->close c)]
                    (if (= top match)
                      (recur (rest ls) (rest stack) completion)
                      (recur (rest ls) stack (conj completion match))))))))

(defn ac-scores [line]
  (reduce (fn [total new]
            (+ new (* total 5)))
          (map #(get ac-score %) line)))

(defn final-ac-score [line]
  (let [half (int (/ (count line) 2))]
    (first (drop half (sort line)))))

(defn task-1 []
  (->> "day10_input.txt"
       resource->input
       (map (partial parse-line (constantly nil) identity))
       (filter identity)
       scores
       (apply +)))

(defn task-2 []
  (->> "day10_input.txt"
       resource->input
       (map (partial parse-line (fn [_ y] y) (constantly nil)))
       (filter identity)
       (map auto-complete)
       (map ac-scores)
       final-ac-score))

(comment
  (= 240123 (task-1))
  (= 3260812321 (task-2))
  ;;marker
  )
