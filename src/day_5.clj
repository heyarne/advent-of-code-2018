(ns day-5
  (:require [clojure.string :as str]))

(def input-polymer (str/trim (slurp "resources/input_5.txt")))
(def example "dabAcCaCBAcCcaDA")

(defn reacting?
  "Tells us whether a unit is reacting"
  [u1 u2]
  (and u1 u2 (= 32 (Math/abs (- (int u1) (int u2))))))

(reacting? \A \a) ;; => true
(reacting? \r \R) ;; => true
(reacting? \b \A) ;; => false

(defn reduce-unit [unit]
  ;; units should be small enough, so parallelizing stuff here doesn't
  ;; really make sense
  (->> (reduce (fn [agg type]
                 (if (reacting? (first agg) type)
                   (rest agg)
                   (conj agg type)))
               '() unit)
       (reverse)))

(defn reduce-polymer
  "Repeatedly causes reactions until the polymer is completely reduced"
  [polymer]
  (loop [polymer polymer]
    (let [units (partition-by #(Character/toLowerCase %) polymer)
          step (mapcat reduce-unit units)]
      (if (= step polymer)
        (str/join polymer)
        (recur step)))))

;; this is the example given on the site:
(reduce-polymer example)
;; => dabCBAcaDA

;; "How many units remain after fully reacting the polymer you scanned?"
(comment
  (time
   (count (reduce-polymer input-polymer))))
;; => 11668
;; => "Elapsed time: 32466.775362 msecs"

(defn remove-type [t polymer]
  (remove #{t (char (- (int t) 32))} polymer))
;; again, trying out the examples

(for [type '(\a \b \c \d)]
  (reduce-polymer
   (remove-type type example)))
;; => ("dbCBcD" "daCAcaDA" "daDA" "abCBAc")

;; looking good :)

(def alphabet (map char (range (int \a) (inc (int \z)))))

(comment)
(time
 (->> (pmap (fn [t]
              [t (count (reduce-polymer (remove-type t input-polymer)))])
            alphabet)
      (sort-by second)
      (last)))
;; => StackOverflowError   clojure.lang.RT.classForName (RT.java:2213)
