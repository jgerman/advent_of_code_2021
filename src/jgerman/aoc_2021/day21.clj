(ns jgerman.aoc-2021.day21)

(def sample {:p1 4 :p2 8})
(def input {:p1 1 :p2 6})

(defn det-d100 [n]
  (if (= n 100)
    100
    (mod n 100)))

(defn move [pos move]
    (let [new (mod (+ pos move) 10)]
      (if (= new 0)
        10
        new)))

(defn play-game [input target]
  (loop [p1 (:p1 input)
         p2 (:p2 input)
         p1-score 0
         p2-score 0
         die-rolls 0
         turn :p1]
    (tap> {:p1 p1 :p2 p2
           :p1-score p1-score :p2-score p2-score
           :die-rolls die-rolls})
    (cond
      (<= target p1-score) (* die-rolls p2-score)
      (<= target p2-score) (* die-rolls p1-score)
      :else (let [roll-result (+ (det-d100 (+ die-rolls 1))
                                 (det-d100 (+ die-rolls 2))
                                 (det-d100 (+ die-rolls 3)))]
              (if (= turn :p1)
                (recur (move p1  roll-result)
                       p2
                       (+ p1-score (move p1 roll-result))
                       p2-score
                       (+ 3 die-rolls)
                       :p2)
                (recur p1
                       (move p2 roll-result)
                       p1-score
                       (+ p2-score (move p2 roll-result))
                       (+ 3 die-rolls)
                       :p1))))))

(def worlds [[3 1] [4 3] [5 6] [6 7] [7 6] [8 3] [9 1]])

(declare play2)
(defn play2* [p1 s1 p2 s2]
  (cond
    (<= 21 s1) [1 0]
    (<= 21 s2) [0 1]
    :else
    (reduce (fn [wins [roll cnt]]
              (let [new-p1 (move p1 roll)
                    new-s1 (+ s1 new-p1)
                    [o m] (play2 p2 s2 new-p1 new-s1)]
                [(+ (first wins) (* m cnt)) (+ (second wins) (* o cnt))]))
            [0 0]
            worlds)))

(def play2 (memoize play2*))

(comment
  (move 1 1)
  (move 4 6)
  (take 11 move)

  (det-d100 100)

  (= 739785 (play-game sample 1000))
  (= 604998 (play-game input 1000))
  (= 157253621231420 (apply max (play2 1 0 6 0)))

  ;; 9065676286260 too low
  ;;
  )
