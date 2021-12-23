(ns jgerman.aoc-2021.day22
  (:require
   [jgerman.aoc-2021.utils :as utils]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.core.matrix :as m]))

(defrecord cuboid [cmd x1 x2 y1 y2 z1 z2])

(defn parse-line [l]
  (let [line (-> l
                 (str/replace #"x=" "")
                 (str/replace #"y=" "")
                 (str/replace #"z=" "")
                 (str/replace #"," " ")
                 (str/replace #"\.\." " ")
                 (str/split #"\s+"))]
    (apply ->cuboid (conj (map #(Integer/parseInt %) (rest line))
                          (= "on" (first line))))))

(defn resource->input [resource]
  (->> resource
       utils/resource->lines
       (map parse-line)))

(def t1-between #(<= -50 % 50))

(defn in-bounds? [c]
  (every? identity
          (map t1-between ((juxt :x1 :x2 :y1 :y2 :z1 :z2) c))))

(defn cuboid->range [c dim1 dim2]
  (if (< (dim1 c) (dim2 c))
    (range (dim1 c) (inc (dim2 c)))
    (range (dim2 c) (inc (dim1 c)))))

;; ugly but does the job... must be a cleaner way
(defn cuboid->points [cuboid]
  (partition 3
             (flatten (for [x (cuboid->range cuboid :x1 :x2)]
                        (for [y (cuboid->range cuboid :y1 :y2)]
                          (for [z (cuboid->range cuboid :z1 :z2)]
                            [x y z]))))))

(defn process-cuboid [cubes-on cuboid]
  (let [process-fn (if (:cmd cuboid) conj disj)]
    (reduce (fn [acc pt]
              (process-fn acc pt))
            cubes-on
            (cuboid->points cuboid))))

(defn do-initialization-procedure [cuboids]
  (reduce (fn [acc c]
            (if (not (in-bounds? c))
              acc
              (process-cuboid acc c)))
          #{}
          cuboids))

(defn cuboid->volume [c]
  (let [x (Math/abs (- (inc (:x2 c)) (:x1 c)))
        y (Math/abs (- (inc (:y2 c)) (:y1 c)))
        z (Math/abs (- (inc (:z2 c)) (:z1 c)))]
    (* x y z)))

(defn cuboids->intersection-sign [c1 c2]
  (let [cmd1 (:cmd c1)
        cmd2 (:cmd c2)]
    (cond
      (and cmd1 cmd2) (not cmd1) ;; they're both "on" so return false
      (and cmd1 (not cmd2)) cmd2 ;; on off generate off
      (and (not cmd1) cmd2) cmd2 ;; off on generate on
      (and (not cmd1) (not cmd2)) (not cmd2)))) ;; off off needs balance

(defn cuboid-intersection [c1 c2]
  (let [x1 (max (:x1 c1) (:x1 c2))
        x2 (min (:x2 c1) (:x2 c2))
        y1 (max (:y1 c1) (:y1 c2))
        y2 (min (:y2 c1) (:y2 c2))
        z1 (max (:z1 c1) (:z1 c2))
        z2 (min (:z2 c1) (:z2 c2))]
    (when (and (< x1 x2)
               (< y1 y2)
               (< z1 z2))
      (->cuboid (cuboids->intersection-sign c1 c2) x1 x2 y1 y2 z1 z2))))

(defn generate-cubes [cubes cube]
  (let [xs (if (:cmd cube) [cube] [])]
    (loop [acc xs
           remaining cubes]
      (if (empty? remaining)
        acc
       (let [intersection (cuboid-intersection (first remaining) cube)]
         (if (nil? intersection)
           (recur acc (rest remaining))
           (recur (conj acc intersection) (rest remaining))))))))

(defn generate-volumes-list [input]
  (loop [volumes [(first input)]
         remaining (rest input)]
    (if (empty? remaining)
      volumes
      (let [new-cubes (generate-cubes volumes (first remaining))]
        (recur (concat volumes new-cubes) (rest remaining))))))

(defn calc-volumes-list [volumes-list]
  (map (fn [c]
         (let [v (cuboid->volume c)]
           (if (:cmd c)
             v
             (* v -1)))) volumes-list))

(defn task-1 [resource]
  (-> resource
      resource->input
      do-initialization-procedure
      count))

(defn task-2 [resource]
  (->> resource
       resource->input
       generate-volumes-list
       calc-volumes-list
       (apply +)))

(comment
  (= 553201 (task-1 "day22_input.txt"))
  (= 1263946820845866 (task-2 "day22_input.txt"))

  ;;marker
  )
