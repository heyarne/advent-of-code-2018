(ns day-3
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.reducers :as r]))

(def input (->> (slurp "resources/input_3.txt")
                (str/split-lines)))

(defrecord Claim [id x y w h])

(defn str->claim [s]
  (let [[_ & matches] (re-matches #"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)]
    (apply ->Claim (map #(Long/parseLong % 10) matches))))

(str->claim (first input))
;; => {:id 1, :x 56, :y 249, :w 24, :h 16}

(def claims (map str->claim input))

(defn space-of-claim [{:keys [x y w h]}]
  (set (for [x (range x (+ x w))
             y (range y (+ y h))]
         [x y])))

(defn coord-map [claims]
  (r/reduce (fn [prev c]
              (merge-with + prev (into {} (for [coord (space-of-claim c)]
                                            [coord 1]))))
            {} claims))

(defn total-overlap [claims]
  (->>
   (coord-map claims)
   (vals)
   (filter (partial < 1))
   (count)))

;; example given on the page
(def example "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")

(let [claims (map str->claim (str/split-lines example))]
  (map (comp count space-of-claim) claims))

(let [claims (map str->claim (str/split-lines example))]
  (total-overlap claims))
;; => 4

;; How many square inches overlap in total?

(total-overlap claims)
;; => 118539
