(ns jgerman.aoc-2021.day4
  (:require [jgerman.aoc-2021.utils :as utils]
            [clojure.string :as str]))

(defn parse-drawings [drawings]
  (vec (map (fn [i] (Integer/parseInt i))
            (-> drawings
                (str/split #",")))))

(defn parse-row [row]
  (let [squares (str/split (str/trim row) #"\s+")]
    (map #(Integer/parseInt %) squares)))

(defn parse-board [board]
  (let [rows (str/split board #"\n")]
    (map parse-row rows)))

(defn parse-boards [boards]
  (map parse-board boards))

(defn resource->input [resource]
  (let [text (utils/resource->text resource)
        chunks (str/split text #"\n\n")]
    {:drawings (parse-drawings (first chunks))
     :boards (parse-boards (rest chunks))}))

;; stolen from day 3 where it has misnamed vars
(defn element-in-pos
  "xs is a collection of collections, returns a sequence of the items at index pos
  from each collection."
  [xs pos]
  (map (fn [s]
         (nth s pos))
       xs))

(defn rotate-board
  "This only works for a square board, but since the input is controlled here..."
  [board]
  (for [idx (range (count (first board)))]
    (element-in-pos board idx)))

(defn coll-covered?
  "Is every item in coll in elements?"
  [coll elements]
  (every? #(some #{%} elements) coll))

(defn board-wins? [board drawn-numbers]
  (let [rotated-board (rotate-board board)]
    (or
     (not-empty (filter #(coll-covered? % drawn-numbers) board))
     (not-empty (filter #(coll-covered? % drawn-numbers) rotated-board)))))

(defn remove-matching [coll matching]
  (filter #(not (some #{%} matching)) coll))

(defn board-score [board drawn-numbers]
  (let [board-elements (flatten board) ;; yeah yeah I know, it's ok here though
        unmarked-elements (remove-matching board-elements drawn-numbers)]
    (* (apply + unmarked-elements)
       (last drawn-numbers))))

(defn find-winning-boards [boards drawn-numbers]
  (filter #(board-wins? % drawn-numbers)
          boards))

(defn find-first-winner [{:keys [drawings boards]}]
  (loop [drawn-numbers [(first drawings)]
         undrawn (rest drawings)]
    (let [winner (first (find-winning-boards boards drawn-numbers))]
      (cond
        winner (board-score winner drawn-numbers)
        (empty? undrawn) -1 ;; could throw an exception
        :else (recur (conj drawn-numbers (first undrawn))
                     (rest undrawn))))))

(defn find-last-winner [{:keys [drawings boards]}]
  (loop [drawn-numbers [(first drawings)]
         undrawn (rest drawings)
         remaining-boards boards]
    (tap> {:remaining-boards (count remaining-boards)
           :drawn-numbers drawn-numbers
           :undrawn-numbers undrawn})
    (let [winners (find-winning-boards remaining-boards drawn-numbers)]
      (cond
        (empty? undrawn) -1
        (= 1 (count remaining-boards)) (find-first-winner {:drawings drawings :boards remaining-boards})
        :else (recur (conj drawn-numbers (first undrawn))
                     (rest undrawn)
                     (remove-matching remaining-boards winners))))))

(defn task1 []
  (-> "day4_input.txt"
      resource->input
      find-first-winner))

(defn task2 []
  (-> "day4_input.txt"
      resource->input
      find-last-winner))

(comment
  (def input (resource->input "day4_sample.txt"))
  (first (:boards input))
  (rotate-board (first (:boards input)))
  (coll-covered? '(1 2 3 4) [2 1 3 5 21 17])
  (coll-covered? '(1 2 3 4) [2 3 4 5 1 6])
  (def sample-drawn-1 [7 4 9 5 11 17 23 2 0 14 21])
  (def sample-drawn-2 [7 4 9 5 11 17 23 2 0 14 21 24])
  (board-wins? (nth (:boards input) 2) sample-drawn-2)
  (nth (:boards input) 2)
  (remove-matching '(1 2 3 6 4 5 6) [2 3 5 6])
  (board-score (nth (:boards input) 2) sample-drawn-2)

  (def sample-input (resource->input "day4_sample.txt"))
  (find-first-winner sample-input)

  ;; tests
  (= 89001
     (task1))
  (= 7296
     (task2))
;; marker
  )
