(ns jgerman.aoc-2021.day8
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

;; what a mess... I'm not proud of this code but it does return instantly
;; this one is a serious candidate for rework, but I got into just grit your teeth and solve it mode
;; it's also a candidate for using core.logic, or a custom implementation of a logic pattern matcher

(defn parse-line [input-line]
  (let [parts (str/split input-line #"\s\|\s")
        patterns (str/split (first parts) #"\s+")
        output (str/split (second parts) #"\s+")]
    {:patterns patterns
     :output output}))

(defn resource->input [resource]
  (->> resource
       utils/resource->lines
       (map parse-line)))

(defn segments->initial-knowns [segments]
  (case (count segments)
    2 {:one {:digit 1 :segments (set segments) :lookup {(set segments) 1}}}
    4 {:four {:digit 4 :segments (set segments) :lookup {(set segments) 4}}}
    3 {:seven {:digit 7 :segments (set segments) :lookup {(set segments) 7}}}
    7 {:eight {:digit 8 :segments (set segments) :lookup {(set segments) 8}}}
    nil))

(defn key->pattern [knowns key]
  (get-in knowns [key :segments]))

(defn segments->diff [p1 p2]
  (first (set/difference p1 p2)))

(defn pattern-contains?
  "Does pattern 1 contain pattern 2... is the set difference empty."
  [p1 p2]
  (let [set1 (set p1)
        set2 (set p2)
        u (set/union set1 set2)]
    (= set1 u)))

;; for convenience
(defn patterns-by-size [patterns size]
  (filter (fn [pattern]
            (= size (count pattern))) patterns))

(defn find-zero
  "There can actually only be two 6 segment patterns."
  [knowns patterns]
  (let [six-segments (distinct (patterns-by-size patterns 6))
        one-pattern (key->pattern knowns :one)
        four-pattern (key->pattern knowns :four)]
    (merge knowns (apply merge (map (fn [pat]
                                      (when (and (pattern-contains? pat one-pattern)
                                                 (not (pattern-contains? pat four-pattern)))
                                        {:zero {:digit 0
                                                :segments (set pat)
                                                :lookup {(set pat) 0}}})) six-segments)))))

(defn find-six
  "There can actually only be two 6 segment patterns."
  [knowns patterns]
  (let [six-segments (distinct (patterns-by-size patterns 6))
        one-pattern (key->pattern knowns :one)
        zero-pattern (key->pattern knowns :zero)]
    (merge knowns (apply merge (map (fn [pat]
                                      (when (and (not (pattern-contains? pat one-pattern))
                                                 (not (pattern-contains? pat zero-pattern)))
                                        {:six {:digit 6
                                               :segments (set pat)
                                               :lookup {(set pat) 6}}})) six-segments)))))

(defn find-nine
  "There can actually only be two 6 segment patterns."
  [knowns patterns]
  (let [six-segments (distinct (patterns-by-size patterns 6))
        six-pattern (key->pattern knowns :six)
        zero-pattern (key->pattern knowns :zero)]
    (merge knowns (apply merge (map (fn [pat]
                                      (when (and (not= pat six-pattern)
                                                 (not= pat zero-pattern))
                                        {:nine {:digit 9
                                                :segments pat
                                                :lookup {pat 9}}})) (map set six-segments))))))

(defn find-five
  "There can actually only be two 6 segment patterns."
  [knowns patterns]
  (let [five-segments (distinct (patterns-by-size patterns 5))
        top-right (get-in knowns [:segments :top-right])]
    (merge knowns (apply merge (map (fn [pat]
                                      (when (not (some #{top-right} pat))
                                        {:five {:digit 5
                                                :segments pat
                                                :lookup {pat 5}}})) (map set five-segments))))))

(defn find-three
  [knowns patterns]
  (let [five-segments (distinct (patterns-by-size patterns 5))
        one (key->pattern knowns :one)]
    (merge knowns (apply merge (map (fn [pat]
                                      (when (pattern-contains? pat one)
                                        {:three {:digit 3
                                                 :segments (set pat)
                                                 :lookup {(set pat) 3}}})) five-segments)))))

(defn find-two
  [knowns patterns]
  (let [five-segments (distinct (patterns-by-size patterns 5))
        five (key->pattern knowns :five)
        three (key->pattern knowns :three)]
    (merge knowns (apply merge (map (fn [pat]
                                      (when (and (not= five pat)
                                                 (not= three pat))
                                        {:two {:digit 2
                                               :segments pat
                                               :lookup {pat 2}}})) (map set five-segments))))))

(defn find-top-right [knowns]
  (let [segments (:segments knowns)
        eight (key->pattern knowns :eight)
        six (key->pattern knowns :six)]
    (merge knowns {:segments (assoc segments :top-right (first (set/difference eight six)))})))

(defn add-top [knowns]
  (merge knowns
         {:segments {:top (set/difference (segments->diff (key->pattern knowns :seven) (key->pattern knowns :one)))}}))

(defn build-initial-knowns [patterns]
  (->> patterns
       (map segments->initial-knowns)
       (filter identity)
       (apply merge)
       add-top))

(defn output->digit-count [output]
  (count (filter segments->initial-knowns output)))

(defn task-1 []
  (->> "day8_input.txt"
       resource->input
       (map :output)
       (map output->digit-count)
       (apply +)))

(defn input->answer [input]
  (let [patterns (:patterns input)
        first-initial (build-initial-knowns patterns)]
    (-> first-initial
        (find-zero patterns)
        (find-six patterns)
        (find-nine patterns)
        find-top-right
        (find-five patterns)
        (find-three patterns)
        (find-two patterns))))

(defn answer-key->digit-lookup [answer-key]
  (apply merge (map :lookup (vals (dissoc answer-key :segments)))))

(defn list->num [ls]
  (+ (* (nth ls 0) 1000)
     (* (nth ls 1) 100)
     (* (nth ls 2) 10)
     (nth ls 3)))

(defn calc-output [answer-key output]
  (list->num (map (fn [pattern]
                    (get answer-key (set pattern))) output)))

(defn task-2 []
  (let [inputs (resource->input "day8_input.txt")]
    (apply + (for [i inputs]
               (calc-output (answer-key->digit-lookup (input->answer i))
                            (:output i))))))

(comment
  (def sample (resource->input "day8_sample.txt"))
  (def first-sample (first sample))

  (patterns-by-size (:patterns first-sample) 6)

  (def first-initial (build-initial-knowns (:patterns first-sample)))
  ;; find zero first
  (def with-zero (merge first-initial (find-zero first-initial (:patterns first-sample))))
  ;; then 6
  (def with-six (merge with-zero (find-six with-zero (:patterns first-sample))))
  ;; then 9
  (def with-nine (merge with-six (find-nine with-six (:patterns first-sample))))
  ;; then top right

  (def with-top-right (merge with-nine (find-top-right with-nine)))

  ;; then five
  (def with-five (merge with-top-right (find-five with-top-right (:patterns first-sample))))

  ;; then three
  (def with-three (merge with-five (find-three with-five (:patterns first-sample))))

  ;; finally 2
  (def with-two (merge with-three (find-two with-three (:patterns first-sample))))

  (input->answer first-sample)
  (def answer-key (answer-key->digit-lookup (input->answer first-sample)))

  (calc-output answer-key (:output first-sample))

  (def example {:patterns ["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"]
                :output ["cdfeb" "fcadb" "cdfeb" "cdbaf"]})

  (calc-output (answer-key->digit-lookup (input->answer example))
               (:output example))

  (map :lookup (vals (dissoc (input->answer example) :segments)))
  (= 504 (task-1))
  (= 1073431 (task-2))
  ;; marker
  )
