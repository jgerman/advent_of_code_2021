(ns jgerman.aoc-2021.day16
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn pad [s]
  (case (count s)
    1 (str "000" s)
    2 (str "00" s)
    3 (str "0" s)
    s))

(defn hex->bin [s]
  (map str (reduce (fn [acc h]
                       (apply conj acc (-> (str h)
                                     (Integer/parseInt 16)
                                     (Integer/toBinaryString)
                                     pad)))
                     []
                     s)))

(defn take-bitstr [in size]
  (apply str (take size in)))

(defn resource->input [resource]
  (-> resource
      utils/resource->text
      str/trim
      hex->bin))

(defn version [in]
  [(Integer/parseInt (take-bitstr in 3) 2) (drop 3 in)])

(defn type-id [in]
  [(Integer/parseInt (take-bitstr in 3) 2) (drop 3 in)])

(declare packet)

(defn literal [in]
  (loop [acc []
         input in]
    (let [bits (apply str (take 5 input))
          rs (drop 5 input)]
      (if (str/starts-with? bits "0")
        [(Long/parseLong (apply str (concat acc (drop 1 bits))) 2) rs]
        (recur (concat acc (drop 1 bits)) rs)))))

(defn parse-type-1 [input]
  (let [num (Integer/parseInt (take-bitstr input 11) 2)
        res (drop 11 input)]
    (loop [acc []
           data res
           idx num]
      (if (= idx 0)
        [acc data]
        (let [[new-packet remaining] (packet data)]
          (recur (conj acc new-packet) remaining (dec idx)))))))

(defn parse-type-0 [input]
  (let [length (Integer/parseInt (take-bitstr input 15) 2)
        res (drop 15 input)
        input-after (drop length res)]
    [(loop [acc []
            data (take length res)]
       (if (empty? data)
         acc
         (let [[new-packet remaining] (packet (take length data))]
           (recur (conj acc new-packet) remaining)))) input-after]))

(defn operator [in]
  (let [length-type-id (first in)]
    (if (= "1" length-type-id)
      (parse-type-1 (rest in))
      (parse-type-0 (rest in)))))

(defn get-value [t input]
  (case t
    4 (literal input)
    (operator input)))

(def versions (atom 0))

(defn packet [in]
  (let [[version res] (version in)
        _ (swap! versions (partial + version))
        [type res] (type-id res)
        [v remaining] (get-value type res)]
    [{:version version
      :type type
      :value v} remaining]))

(defn eval-tree [n]
  (let [type (:type n)
        v (:value n)]
    (case type
      0 (apply + (map eval-tree v))
      1 (apply * (map eval-tree v))
      2 (apply min (map eval-tree v))
      3 (apply max (map eval-tree v))
      4 v
      5 (if (> (eval-tree (first v)) (eval-tree (second v))) 1 0)
      6 (if (< (eval-tree (first v)) (eval-tree (second v))) 1 0)
      7 (if (= (eval-tree (first v)) (eval-tree (second v))) 1 0))))

(defn task-1 [resource]
  (-> resource
      resource->input
      packet
      first))

(comment
  ;; op 0 test
  (packet (hex->bin "38006F45291200"))
  ;; op 1 test
  (packet (hex->bin "EE00D40C823060"))
  (or (> 2 1) 1 0)
  ;; 1012 -- yes I cheesed this with an atom
  (reset! versions 0)
  (= 2223947372407 (eval-tree (task-1 "day16_input.txt")))
  @versions

;;
  )
