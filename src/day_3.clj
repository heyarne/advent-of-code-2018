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

(defn claim->coords [claim]
  (into {} (for [coord (space-of-claim claim)] [coord 1])))

(defn coord-map
  "Gives us a map of all occupied coordinates given a sequence of claims"
  [claims]
  (r/reduce (fn [prev c]
              (merge-with + prev (claim->coords c)))
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

;; What is the one claim that is not overlapped?

(let [#_#_ claims (map str->claim (str/split-lines example))
      ;; use this for faster (constant instead of linear) lookup
      coords->claim (->>
                     (map (fn [c]
                            [[(:x c) (:y c)] c])
                          claims)
                     (into {}))
      ;; coords only occupied by one claim
      sparse-coords (->> (filter #(= 1 (val %)) (coord-map claims))
                         (into {}))]
  ;; now for all coordinates that are occupied by only one claim,
  ;; we check the claim that fits into these coordinates entirely
  (->> (keep (fn [[coords _]]
               (when-let [claim (coords->claim coords)]
                 (when (->> (claim->coords claim)
                            (map (fn [[coords _]] coords))
                            (every? sparse-coords))
                   claim)))
             sparse-coords)
       (first)))
;; => {:id 1270, :x 221, :y 290, :w 24, :h 16}
