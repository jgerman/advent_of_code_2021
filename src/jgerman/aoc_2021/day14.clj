(ns jgerman.aoc-2021.day14
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))


(defn input-string->pairs [s]
  (loop [in s
         pairs []]
    (let [c (first in)
          rs (rest in)]
      (if (nil? c)
        pairs
        (recur rs (conj pairs [c (first rs)]))))))

(defn build-rules [rules]
  (reduce merge {} (map (fn [r]
                          (let [[match replace] (str/split r #" -> ")]
                            {[(first match) (second match)] (first replace)})) rules)))

(defn build-input [[input-string rules]]
  {:input-string input-string
   :rules (build-rules (str/split rules #"\n"))})

(defn resource->input [resource]
  (-> resource utils/resource->text (str/split #"\n\n") build-input))

(defn match [rules pair]
  (if-let [m (get rules pair)]
    [[(first pair) m] [m (second pair)]]
    [pair]))

(defn fire-rules [rules pairs]
  (loop [acc []
         input pairs]
    (let [curr (first input)]
      (if (nil? curr)
        acc
        (recur (into acc (match rules curr)) (rest input))))))

;; this could probably be a reduce for a lazy seq
(defn do-steps [rules pairs steps]
  (loop [acc pairs
         step 0]
    (cond
      (= step steps) acc
      :else (recur (fire-rules rules acc) (inc step)))))

(defn task-1 [resource steps]
  (let [input (resource->input resource)
        pairs (input-string->pairs (:input-string input))
        final (map first (do-steps (:rules input) pairs steps))
        freqs (frequencies final)]
    (- (apply max (vals freqs))
       (apply min (vals freqs)))))

(comment
  (resource->input "day14_sample.txt")
  (input-string->pairs (:input-string (resource->input "day14_sample.txt")))

  (task-1 "day14_sample.txt" 10)

  (= 3259 (task-1 "day14_input.txt" 10))
  (= 1234 (task-1 "day14_input.txt" 40))
  ;; marker
  )
