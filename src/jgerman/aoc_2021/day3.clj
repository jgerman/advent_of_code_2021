(ns jgerman.aoc-2021.day3
  (:require [jgerman.aoc-2021.utils :as utils]))

(defn char-in-pos [strings pos]
  (map (fn [s]
         (nth s pos))
       strings))

(defn string-has-char-in-pos? [string c pos]
  (= c
     (nth string pos)))

;; there's an assumption in the first task that the values
;; aren't ever equal, either 1 or 0 is more common
;;
;; in the second task of the day we need to return a partuclar value
;; when they're equal so I refactored to account for that, which shouldn't break task 1
(defn find-most-common [strings pos]
  (let [chars (char-in-pos strings pos)
        total (count strings)
        ones (count (filter #(= \1 %) chars))
        zeroes (- total ones)]
    (cond
      (= zeroes ones) \=
      (< zeroes ones) \1
      :else \0)))

;; we don't actually have to calculate gamma if we have epsilon
;; or vice versa, but defined this way we can get either at any time
(defn complement-binstring [string]
  (apply str
         (map (fn [c]
                (if (= \1 c)
                  \0
                  \1))
              string)))

(defn calc-gamma [readings]
  (apply str
         (for [idx (range (count (first readings)))]
           (find-most-common readings idx))))

(defn calc-epsilon [readings]
  (complement-binstring (calc-gamma readings)))

(defn bit-criteria-most-common [readings pos]
  (let [most-common (find-most-common readings pos)]
    (if (= most-common \=)
      \1
      most-common)))

(defn bit-criteria-least-common [readings pos]
  (let [most-common (find-most-common readings pos)]
    (case most-common
      \= \0
      \1 \0
      \0 \1)))

;; a lot of this needs to be refactored to just take first class bit criteria fns
;; this is overly verbose
(defn filter-by-most-common [readings pos]
  (let [most-common (bit-criteria-most-common readings pos)]
    (filter (fn [s]
              (string-has-char-in-pos? s most-common pos))
            readings)))

(defn filter-by-least-common [readings pos]
  (let [least-common (bit-criteria-least-common readings pos)]
    (filter (fn [s]
              (string-has-char-in-pos? s least-common pos))
            readings)))

(defn calc-oxygen-generator-rating [readings]
  (loop [remaining readings
         pos 0]
    (if (= 1 (count remaining))
      (first remaining)
      (recur (filter-by-most-common remaining pos) (inc pos)))))

(defn calc-co2-scrubber-rating [readings]
  (loop [remaining readings
         pos 0]
    (if (= 1 (count remaining))
      (first remaining)
      (recur (filter-by-least-common remaining pos) (inc pos)))))

(defn power-consumption [readings]
  (let [gamma (calc-gamma readings)
        epsilon (calc-epsilon readings)
        o2-generator-rating (calc-oxygen-generator-rating readings)
        co2-scrubber-rating (calc-co2-scrubber-rating readings)
        int-gamma (utils/bit-string->int gamma)
        int-epsilon (utils/bit-string->int epsilon)
        int-o2-generator-rating (utils/bit-string->int o2-generator-rating)
        int-co2-scrubber-rating (utils/bit-string->int co2-scrubber-rating)]
    {:gamma gamma
     :epsilon epsilon
     :oxygen-generator-rating o2-generator-rating
     :co2-scrubber-rating co2-scrubber-rating
     :int-gamma int-gamma
     :int-epsilon int-epsilon
     :int-oxygen-generator-rating int-o2-generator-rating
     :int-co2-scrubber-rating int-co2-scrubber-rating
     :power-consumption (* int-gamma int-epsilon)
     :life-support-rating (* int-o2-generator-rating int-co2-scrubber-rating)}))


(defn task []
  (-> "day3_input.txt"
      utils/resource->lines
      power-consumption))

;; tests
(comment
  (= 845186
     (:power-consumption (task)))
  (= 4636702
     (:life-support-rating (task)))
  ,)
