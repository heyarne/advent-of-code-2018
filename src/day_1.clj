(ns day-1
  (:require [clojure.string :as str]))

;; this is developed in emacs w/ cider
;; the result comes from cider-pprint-eval-last-sexp-to-comment

;; finding the final frequency

(def shifts (->> (slurp "resources/input_1.txt")
                 (str/split-lines)
                 (map #(Long/parseLong %))))

(reduce + shifts) ;; => 490

;; finding the first duplicate frequency

(loop [frequency 0
       remaining (cycle shifts)
       frequencies #{}]
  (if (frequencies frequency)
    frequency
    (recur (+ frequency (first remaining))
           (rest remaining)
           (conj frequencies frequency)))) ;; => 70357
