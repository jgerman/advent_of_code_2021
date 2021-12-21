(ns jgerman.aoc-2021.day20
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]
            [clojure.core.matrix :as m]))

(defn process-image-algorithm [s]
  (-> s
      (str/replace #"#" "1")
      (str/replace #"\." "0")))

(defn build-matrix
  ([rows cols] (build-matrix rows cols 0))
  ([rows cols fill]
   (-> (m/new-matrix rows cols)
       (m/fill fill))))

(defn setup-start-map
  "Turn an initial matrix into 1's and 0's."
  [initial]
  (let [image (apply build-matrix (m/shape initial))]
    (reduce (fn [acc [row col]]
              (if (= "#" (m/mget initial row col))
                (m/mset acc row col 1)
                acc))
            image
            (m/index-seq initial))))

(defn process-input-image [xs]
  (setup-start-map (map #(str/split % #"") xs)))

(defn resource->input [resource]
  (let [lines (-> resource
                  utils/resource->lines)
        image-alg (process-image-algorithm (str/trim (first lines)))
        input-image (process-input-image (drop 2 lines))]
    {:algorithm image-alg
     :initial-state input-image}))

(defn print-matrix [matrix]
  (println "-----------------------------")
  (let [[rows columns] (m/shape matrix)]
    (doseq [r (range rows)]
      (doseq [c (range columns)]
        (if (= 1 (m/mget matrix r c))
          (print "#")
          (print ".")))
      (println ""))))

(def scale 3)
(defn grow-image [image oob]
  (let [[rows cols] (m/shape image)
        new-image (build-matrix (+ rows (* 2 scale))
                            (+ cols (* 2 scale)) oob)]
    (tap> {:new-size (m/shape image)})
    (reduce (fn [acc [r c]]
              (m/mset acc (+ r scale) (+ c scale) (m/mget image r c)))
            new-image
            (m/index-seq image))))

(defn safe-mget [image oob r c]
  (let [[rows cols] (m/shape image)]
    (if (or (<= rows r)
            (<= cols c)
            (< r 0)
            (< c 0))
      oob
      (m/mget image r c))))

(defn point->output-idx-str [image oob r c]
  (let [sget (partial safe-mget image oob)]
    (apply str [(sget (dec r)  (dec c))
                (sget (dec r)  c)
                (sget (dec r)  (inc c))
                (sget r        (dec c))
                (sget r        c)
                (sget r        (inc c))
                (sget (inc r)  (dec c))
                (sget (inc r)  c)
                (sget (inc r)  (inc c))])))

(defn point->output-idx [image oob r c]
  (Integer/parseInt (point->output-idx-str image oob r c) 2))

(defn output-pixel [alg idx]
  (Integer/parseInt (str (nth alg idx))))

(defn step [image alg oob]
  (let [new-image (grow-image image oob)]
    (m/emap-indexed (fn [[r c] _]
                      (output-pixel alg (point->output-idx new-image oob r c)))
                    new-image)))

(defn step->oob [alg step] (if (even? step) 0 (output-pixel alg 0)))

(defn do-steps [image alg steps]
  (loop [idx 0
         acc image]
    (if (= idx steps)
      acc
      (recur (inc idx) (step acc alg (step->oob alg idx))))))

(defn task-1 [resource]
  (let [input (-> resource
                  resource->input)]
    (m/esum (do-steps (:initial-state input)
                      (:algorithm input)
                      2))))

(defn task-2 [resource]
  (let [input (-> resource
                  resource->input)]
    (m/esum (do-steps (:initial-state input)
                      (:algorithm input)
                      50))))


(comment
  (= 35 (task-1 "day20_sample.txt"))
  (= 5326 (task-1 "day20_sample2.txt"))
  (= 5339 (task-1 "day20_input.txt"))
  (= 18395 (task-2 "day20_input.txt"))

  ;; 6541 too high
;;marker
  )
