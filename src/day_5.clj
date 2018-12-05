(ns day-5)

(def input-polymer (slurp "resources/input_5.txt"))
(def example "dabAcCaCBAcCcaDA")

(defn reacting?
  "Tells us whether a unit is reacting"
  [u1 u2]
  (and (char? u1) (char? u2)
       (= 32 (Math/abs (- (int u1) (int u2))))))

(reacting? \A \a) ;; => true
(reacting? \r \R) ;; => true
(reacting? \b \A) ;; => false

(defn reduce-unit [unit]
  (reduce (fn [agg type]
            (if (reacting? (last agg) type)
              (vec (butlast agg))
              (conj agg type)))
          [(first unit)] (rest unit)))

(defn reduce-polymer
  "Repeatedly causes reactions until the polymer is completely reduced"
  [polymer]
  (loop [polymer polymer]
    (let [units (partition-by #(Character/toLowerCase %) polymer)
          step (flatten (map reduce-unit units))]
      (if (= step polymer)
        (apply str polymer)
        (recur step)))))

;; this is the example given on the site:
(reduce-polymer example)
;; => dabCBAcaDA

;; "How many units remain after fully reacting the polymer you scanned?"
(count (reduce-polymer input-polymer))
;; => 11669
;; wrong answer :/
