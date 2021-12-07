(ns jgerman.aoc-2021.day6
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn resource->input [resource]
  (frequencies (map #(Integer/parseInt %)
                    (-> resource
                        utils/resource->text
                        str/trim
                        (str/split #",")))))

(defn age-fish [[age num]]
  (let [new-age (dec age)]
    (if (= -1 new-age)
      {6 num
       8 num}
      {new-age num})))

(defn step [fish]
  (->> fish
       seq
       (map age-fish)
       (apply (partial merge-with +))))

(defn sim [fish steps]
  (loop [fs fish
         idx 0]
    (if (= idx steps)
      fs
      (recur (step fs) (inc idx)))))

(defn count-fish [fish]
  (apply + (vals fish)))

(defn task-1 [days]
  (-> "day6_input.txt"
      resource->input
      (sim days)
      count-fish))

(comment
  (def sample (resource->input "day6_sample.txt"))
  (step sample)
  (= 343441 (task-1 80))
  (= 1569108373832 (task-1 256))

;; marker
  )
