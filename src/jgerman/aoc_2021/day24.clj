(ns jgerman.aoc-2021.day24
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn parse-lines [l]
  (str/split (str/trim l) #" "))

(defn resource->input [resource]
  (->> resource
       utils/resource->lines
       (map parse-lines)))

(def d1
  "inp w
mul x 0
add x z
mod x 26
div z 1
add x 12
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 4
mul y x
add z y")

(def d2
  "inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 11
mul y x
add z y")

(defn string->input [s]
  (->> s
       str/split-lines
       (map parse-lines)))

(defn register? [v] (some #{v} ["w" "x" "y" "z"]))

(defn val-of [state v]
  (if (register? v)
    (get state (keyword v))
    (Integer/parseInt v)))

(defn inp [{:keys [w x y z input] :as state} arg]
  (assoc state (keyword (keyword arg)) (first input) :input (rest input)))

(defn alu-add [state a1 a2]
  (assoc state (keyword a1) (+ (val-of state a1) (val-of state a2))))

(defn alu-mul [state a1 a2]
  (assoc state (keyword a1) (* (val-of state a1) (val-of state a2))))

(defn alu-div [state a1 a2]
  (assoc state (keyword a1) (int (/ (val-of state a1) (val-of state a2)))))

(defn alu-mod [state a1 a2]
  (assoc state (keyword a1) (int (mod (val-of state a1) (val-of state a2)))))

(defn alu-eql [state a1 a2]
  (if (= (val-of state a1) (val-of state a2))
    (assoc state (keyword a1) 1)
    (assoc state (keyword a1) 0)))

(defn run-alu [instructions input]
  (loop [s {:w 0 :x 0 :y 0 :z 0 :input (map #(Integer/parseInt (str %)) input)}
         is instructions]
    (when (= "inp" (first (first is))) (tap> {:is (first is)
                                              :state s}))
    (if (empty? is)
      s
      (let [curr (first is)]
        (case (first curr)
          "inp" (recur (inp s (second curr)) (rest is))
          "add" (recur (apply alu-add s (rest curr)) (rest is))
          "mul" (recur (apply alu-mul s (rest curr)) (rest is))
          "div" (recur (apply alu-div s (rest curr)) (rest is))
          "mod" (recur (apply alu-mod s (rest curr)) (rest is))
          "eql" (recur (apply alu-eql s (rest curr)) (rest is)))))))


(defn find-matching []
  (let [instructions (resource->input "day24_input.txt")]
    (doseq [i (range 1111111111)]
      (when (= 0 (:z (run-alu instructions (format "%014d" i))))
        (println "Found 1! " i)))))


(comment
  (def instructions (resource->input "day24_input.txt"))
  (def is3x (resource->input "day24_sample3x.txt"))
  (def bin (resource->input "day24_sample_bin.txt"))
  (run-alu is3x "30")
  (run-alu bin "8")

  (run-alu instructions "")
  (format "%014d" 234)
  (find-matching)
  (mod 0 26)

  (def instr1 (string->input d1))
  (run-alu instr1 "9")
  (def s1 {:w 0 :x 1 :y 5 :z 5})
  (def instr2 (string->input d2))
  (run-alu instr2 "1")
  (* 26 2133463742923)
  ;; marker
  )
