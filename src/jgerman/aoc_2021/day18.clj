(ns jgerman.aoc-2021.day18
  (:require [instaparse.core :as insta]
            [clojure.zip :as zip]))

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

(defn print-tree [t]
  (println "----------------")
  (loop [loc (zip/vector-zip t)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
              (do
                  (println (str (zip/node loc) ":" (depth loc)))
                  loc))))))

(defn node-plus [n amt]
  (zip/edit n + amt))

(defn snail-splode [t]
  (println "----------------")
  (loop [loc (zip/vector-zip t)]
    (if (zip/end? loc)
      (zip/root loc)
      (if (and (= 5 (depth loc))
               (zip/branch? loc)
               (= 2 (count (zip/children loc)))
               (and (not (zip/branch? (zip/down loc))) (not (zip/branch? (zip/next (zip/down loc))))))
        (let [left (zip/node (zip/next loc))
              right (zip/node (zip/next (zip/next loc)))
              zero-node (zip/replace loc 0)]
          (tap> {:left left :right right})
          (if-let [previous-node (previous-num zero-node)]
            ;; previous
            (zip/root
             (let [new-previous (zip/edit previous-node + left)]
               (if-let [new-next (next-num (zip/next new-previous))]
                 ;; there's a next
                 (zip/edit new-next + right)
                 ;; there's no next
                 new-previous)))
            ;; no previous
            (if-let [new-next (next-num zero-node)]
              ;; there's a next
              (zip/root (zip/edit new-next + right))
              ;; there's no next)
              (zip/root zero-node))))
        (recur (zip/next loc))))))

(defn snail-split [t]
  (loop [loc (zip/vector-zip t)]
    (if (zip/end? loc)
      (zip/root loc)
      (if (and (not (zip/branch? loc))
               (<= 10 (zip/node loc)))
        (zip/root
         (zip/edit loc split-num))
        (recur (zip/next loc))))))

(comment
  (def example (read-string "[[[[[9,8],1],2],3],4]"))
  (def example2 (read-string "[7,[6,[5,[4,[3,2]]]]]"))
  (def example3 (read-string "[[6,[5,[4,[3,2]]]],1]"))
  (def example4 (read-string "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
  (def example5 (read-string "[[3,[10,[8,0]]],[9,[23,[4,[3,2]]]]]"))
  (def z (zip/vector-zip (read-string "[[[[[9,8],1],2],3],4]")))
  (-> z
      zip/down zip/node)

  (-> z
      zip/next zip/next zip/node)

  (-> z
      zip/next zip/next zip/next zip/node)

  (snail-splode example)

  (snail-split example5)



  (split-num 11)
;; marker
  )
