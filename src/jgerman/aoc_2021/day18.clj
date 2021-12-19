(ns jgerman.aoc-2021.day18
  (:require [instaparse.core :as insta]
            [clojure.zip :as zip]
            [jgerman.aoc-2021.utils :as utils]))

;; here's an example that supposedly works: https://github.com/tcsullivan/advent-of-code/blob/master/day18/partboth.clj
;; I'm at the point where I'm not learning and re-implementing I'm cribbing

#_(def snail
  (insta/parser
   "<S> = PAIR
    PAIR = <'['> L <','> R <']'>
    <L> = num | PAIR
    <R> = num | PAIR
    num = #'[0-9]+'"))

#_(def num-txfm {:num (fn [x] {:num (Integer/parseInt x)})
               :PAIR (fn [l r] {:type :pair :left l :right r})})

(defn split-num [n]
  (mapv long [(Math/floor (/ n 2)) (Math/ceil (/ n 2))]))

(defn depth [loc]
  (loop [n loc
         d 0]
    (if (nil? n)
      d
      (recur (zip/up n) (inc d)))))

(defn previous-num [loc]
  (loop [n loc]
    (if-let [previous (zip/prev n)]
      (if (not (zip/branch? previous))
        previous
        (recur previous)))))

(defn next-num [loc]
  (when (nil? loc) nil)
  (loop [n loc]
    (if-let [n-loc (zip/next n)]
      (cond
        (zip/end? n-loc) nil
        (not (zip/branch? n-loc)) n-loc
        :else (recur n-loc)))))

(defn pair-node? [n]
  (and (zip/branch? n)
       (= 2 (count (zip/children n)))
       (and (not (zip/branch? (zip/down n))) (not (zip/branch? (zip/next (zip/down n)))))
       #_(not (zip/branch? (first (zip/children n))))
       #_(not (zip/branch? (second (zip/children n))))))

(defn reduce-pair [t]
  (loop [loc (zip/vector-zip t)
         reduced-it false]
    (if (zip/end? loc)
      [(zip/root loc) reduced-it]
      (if (pair-node? loc)
        [(zip/root
          (zip/replace loc (+ (* 3 (first (zip/node loc))) (* 2 (second (zip/node loc)))))) true]
        (recur (zip/next loc) reduced-it)))))

(defn magnitude [t]
  (loop [ps t
         continue true]
    (if continue
      (let [[new-pairs reduce-it] (reduce-pair ps)]
        (recur new-pairs reduce-it))
      ps)))

(defn node-plus [n amt]
  (zip/edit n + amt))

(defn snail-splode [t]
  (loop [loc (zip/vector-zip t)
         exploded false]
    #_(tap> {:node (zip/node loc)
           :dept (depth loc)})
    (if (zip/end? loc)
      [(zip/root loc) exploded]
      (if (and (= 5 (depth loc))
               (zip/branch? loc)
               (= 2 (count (zip/children loc)))
               (and (not (zip/branch? (zip/down loc))) (not (zip/branch? (zip/next (zip/down loc))))))
        (let [left (zip/node (zip/next loc))
              right (zip/node (zip/next (zip/next loc)))
              zero-node (zip/replace loc 0)]
          (if-let [previous-node (previous-num zero-node)]
            ;; previous
            [(zip/root
              (let [new-previous (zip/edit previous-node + left)]
                #_(tap> {:updating (zip/node previous-node)
                       :left left})
                (if-let [new-next (next-num (next-num new-previous))]
                  ;; there's a next
                  (do #_(tap> {:previous true
                             :updating-next true
                             :updating (zip/node new-next)
                             :right right})
                      (zip/edit new-next + right))
                  ;; there's no next
                  (do #_(tap> {:no-next true})
                      new-previous)))) true]
            ;; no previous
            (if-let [new-next (next-num zero-node)]
              ;; there's a next
              [(zip/root (zip/edit new-next + right)) true]
              ;; there's no next)
              [(zip/root zero-node) true])))
        (recur (zip/next loc) exploded)))))

(defn snail-split [t]
  (loop [loc (zip/vector-zip t)
         split false]
    (if (zip/end? loc)
      [(zip/root loc) split]
      (if (and (not (zip/branch? loc))
               (<= 10 (zip/node loc)))
        [(zip/root
          (zip/edit loc split-num)) true]
        (recur (zip/next loc) split)))))

#_(defn snail-explode-cycle [exp]
  (loop [snail-exp exp
         cont true]
    (if cont
      (let [[res exploded]])
      snail-exp)))

(defn snail-reduce [exp]
#_  (println "Reducing: " exp)
  (loop [snail-exp exp
         cont true]
    (if cont
      (let [ex-cycle (loop [explode-exp snail-exp
                            more-explodes true]
                       (if more-explodes
                         (let [[res exploded] (snail-splode explode-exp)]
                           #_(when exploded (println "exploded:" res))
                           (recur res exploded))
                         explode-exp))]
        (let [[split-res did-split] (snail-split ex-cycle)]
#_          (when did-split (println "splitted:" split-res))
          (recur split-res did-split)))
      snail-exp)))

(defn permutations [x xs]
  (concat (for [v xs]
            [x v])
          (for [v xs]
            [v x])))

(defn all-permutations
  ([xs]
   (all-permutations [] (first xs) (rest xs)))
  ([acc x xs]
   (if (nil? x)
     acc
     (all-permutations (concat acc (permutations x xs)) (first xs) (rest xs)))))

#_(all-permutations  [[1] [2] [3] [4]])
#_(all-permutations [1 2 3 4])

#_(permutations (->> "day18_sample.txt"
                   utils/resource->lines
                   (map read-string)))

(defn snail-add [exp1 exp2]
  (snail-reduce [exp1 exp2]))

(defn snail-solve [exps]
  (reduce snail-add exps))

(defn add-all-pairs [ps]
  (map (fn [[x y]]
         (magnitude (snail-add x y))) ps))

(defn task-1 [resource]
  (->> resource
       utils/resource->lines
       (map read-string)
       snail-solve))

(defn task-2 [resource]
  (->> resource
       utils/resource->lines
       (map read-string)
       all-permutations
       add-all-pairs
       (apply max)))

(comment
  (def example (read-string "[[[[[9,8],1],2],3],4]"))
  (def example2 (read-string "[7,[6,[5,[4,[3,2]]]]]"))
  (def example3 (read-string "[[6,[5,[4,[3,2]]]],1]"))
  (def example4 (read-string "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
  (def example5 (read-string "[[3,[10,[8,0]]],[9,[23,[4,[3,2]]]]]"))
  (def example6 (read-string "[[[[4,0],[5,4]],[[7,7],[0,13]]],[10,[[11,9],[11,0]]]]"))
  (def z (zip/vector-zip (read-string "[[[[[9,8],1],2],3],4]")))
  (-> z
      zip/down zip/node)

  (-> z
      zip/next zip/next zip/node)

  (-> z
      zip/next zip/next zip/next zip/node)

  (snail-splode example6)

  (snail-split example5)

  (snail-reduce example5)
  (println "--------------------------------------")
  (println "Result: " (snail-reduce (read-string "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]")))
  (println "Result: " (task-1 "day18_sample.txt"))

  (reduce-pair (task-1 "day18_sample.txt"))
  (= 4140 (magnitude (task-1 "day18_sample.txt")))
  (= 3524 (magnitude (task-1 "day18_input.txt")))

  (= 3993 (task-2 "day18_sample.txt"))
  (= 3993 (task-2 "day18_input.txt"))

  (defn rs [s] (read-string s))
  (def start (rs "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"))
  (def explode-1 (rs "[[[[4,0],[5,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"))
  (def explode-2 (rs "[[[[4,0],[5,4]],[[0,[7,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"))
  (def start-result (rs "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
  ;; step 1
  (= explode-1
     (-> start
         snail-splode
         first))

  (= explode-2
     (first (-> explode-1
                snail-splode)))

  (= start-result (snail-reduce start))

;; marker
  )
