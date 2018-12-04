(ns day-2
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/input_2.txt")
                (str/split-lines)))

(defn checksum [box-ids]
  (let [[twos threes] (->> (map frequencies box-ids)
                           ;; keep all frequency maps that contain either 2 or 3
                           ;; and keep them exactly once
                           (mapcat #(keep (set (vals %)) [2 3]))
                           (sort)
                           ;; after this we end up with a list like ((2 2 2 2)
                           ;; (3 3 3 3 3)) and count that
                           (partition-by identity))]
    (* (count twos) (count threes))))

(checksum input)
;; => 6888

(defn common-letters [a b]
  (remove nil? (map #(when (= %1 %2) %1) a b)))

(->> (mapcat (fn [a]
               (for [i input]
                 (let [common (common-letters a i)]
                   (when (= (count common) (dec (count a)))
                     common))))
             input)
     (remove nil?)
     (first)
     (apply str))
;; => icxjvbrobtunlelzpdmfkahgs
