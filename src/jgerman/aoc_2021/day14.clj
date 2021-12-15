(ns jgerman.aoc-2021.day14
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

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
                            {[(first match) (second match)] [[(first match) (first replace)]
                                                             [(first replace) (second match)]]})) rules)))

(defn build-input [[input-string rules]]
  {:input-string input-string
   :rules (build-rules (str/split rules #"\n"))})

(defn resource->input [resource]
  (-> resource utils/resource->text (str/split #"\n\n") build-input))

(defn calc [state]
  (let [freqs
        (vals (reduce (fn [acc [[ele _] cnt]]
                        (update acc ele (fn [v] ((fnil + 0) v cnt))))
                      {}
                      (seq state)))]
    (- (apply max freqs) (apply min freqs))))

(defn step [rules state]
  (reduce (fn [acc [pair cnt]]
            (if-let [match (get rules pair)]
              (-> acc
                  (update (first match) (fn [v] ((fnil + 0) v cnt)))
                  (update (second match) (fn [v] ((fnil + 0) v cnt))))
              (assoc acc pair cnt)))
          {}
          (seq state)))

(defn steps [rules state n]
  (loop [acc state
         s 0]
    (if (= s n)
      acc
      (recur (step rules acc) (inc s)))))

(defn task-1 [resource n]
  (let [input (resource->input resource)
        pairs (input-string->pairs (:input-string input))
        state (reduce (fn [m k] (assoc m k 1)) {} pairs)]
    (calc (steps (:rules input) state n))))

(comment
  (= 3259 (task-1 "day14_input.txt" 10))
  (= 3459174981021 (task-1 "day14_input.txt" 40))

  ;; marker
  )

;;20,890,720,927,745
