(ns poker-scoring.core
  (:gen-class)
  (require [clojure.core :refer :all])
  (require [clojure.java.io :as io])
  (require [clojure.string :as str]))

;;; Premade poker hands used while testing. Didn't have time to write real tests.
(def poker-hand
  '("QH" "JH" "AH" "10H" "KH"))

(def straight
  '("QH" "JH" "AS" "10H" "KH"))

(def straight-flush
  '("QH" "JH" "AH" "10H" "KH"))

(def full-house
  '("QH" "3H""QC" "3D" "3S"))

(def four-kind
  '("AC" "AH" "3C" "AD" "AS"))

(def two-pair
  '("4H" "3H" "3C" "4D" "2S"))

(def high-card
  '("4H" "AH""QC" "JD" "KS"))

(def high-card2
  '("3H" "AH""QC" "JD" "KS"))

(def pair
  '("2H" "3H""QC" "6D" "2S"))

(def flushh
  '("2H" "3H""QH" "6H" "10H"))

(def three-kind
  '("3S" "AC" "AH" "AS" "5H"))

(def three-kind2
  '("4H" "AH" "4C" "4D" "KS"))

;;; What happens when both cards are straights of same sequence?
;;; Turns out the data for this test was cleaned to ensure this never happens.
(def player-one (atom 0))
(def player-two (atom 0))

(defn get-card-value
  "takes a card and return the value of card without the suit"
  [card]
  (as-> (subs card 0 (- (count card) 1)) card-rank
    (case card-rank
      "T" 10
      "J" 11
      "Q" 12
      "K" 13
      "A" 14
      (read-string card-rank))))

(def hand-value-map {:high-card      1
                     :pair           2
                     :two-pair       3
                     :three-kind     4
                     :straight       5
                     :flush          6
                     :full-house     7
                     :four-kind      8
                     :straight-flush 9
                     :royal-flush    10})

(defn create-card-frequency-list
  "takes a hand an returns a list with a card number of times a card appears"
  [hand]
  (frequencies (map get-card-value hand)))

(defn flush? [hand]
  (if (= (-> (map last hand)
             set
             count)
         1)
    :flush
    false))

(defn ascending-seq? [x]
  (every? (fn [x] (= x 1)) (map - (rest x) x)))

(defn straight? [hand]
  (if (-> (map get-card-value hand)
          sort
          ascending-seq?)
    :straight
    false))

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    :straight-flush
    false))

(defn highest-card-value
  "returns the value of the highest card. Used for identifying royal flush."
  [hand]
  (-> (map get-card-value hand)
      sort
      last))

(defn royal-flush? [hand]
  (if (and (straight-flush? hand)
           (= (highest-card-value hand) 14))
    :royal-flush
    false))

(defn pair? [freq-list]
  (if (= (->> (vals freq-list)
              (filter (fn [x] (if (= x 2)
                                x
                                nil)))
              count)
         1)
    :pair
    false))

(defn two-pair? [freq-list]
  (if (= (->> (vals freq-list)
              (filter (fn [x] (if (= x 2)
                                x
                                nil)))
              count)
         2)
    :two-pair
    false))

(defn three-kind? [freq-list]
  (if (= (->> (vals freq-list)
              (filter (fn [x] (if (= x 3)
                                x
                                nil)))
              count)
         1)
    :three-kind
    false))

(defn four-kind? [freq-list]
  (if (= (->> (vals freq-list)
              (filter (fn [x] (if (= x 4)
                                x
                                nil)))
              count)
         1)
    :four-kind
    false))

(defn full-house? [freq-list]
  (if (and (not (false? (pair? freq-list)))
           (not (false? (three-kind? freq-list))))
    :full-house
    false))

;;; I don't like the way this function is written,
;;; maybe I could put this in a case statement.
(defn check-for-five-card-combos [hand]
  (if (royal-flush? hand)
    (royal-flush? hand)
    (if (straight-flush? hand)
      (straight-flush? hand)
      (if (straight? hand)
        (straight? hand)
        (if (flush? hand)
          (flush? hand)
          :high-card)))))

(defn check-two-pair-or-three-kind [freq-list]  (if (three-kind? freq-list)
    (three-kind? freq-list)
    (two-pair? freq-list)))

(defn check-full-house-or-four-kind [freq-list]
  (if (four-kind? freq-list)
    (four-kind? freq-list)
    (full-house? freq-list)))

(defn determine-hand
  "Find the number of unique cards in a hand and apply the corrosponding function"
  [hand]
  (let [card-frequency (create-card-frequency-list hand)
        unique-cards (count card-frequency)]
    (case unique-cards
      5 (check-for-five-card-combos hand)
      4 (pair? card-frequency)
      3 (check-two-pair-or-three-kind card-frequency)
      2 (check-full-house-or-four-kind card-frequency))))

(defn move-higher-pair-to-front [freq-list]
  (let [first-card (first freq-list)
        second-card     (second freq-list)]
    (if (< (first first-card) (first second-card))
      (concat (list second-card) (list first-card) (list (last freq-list)))
      freq-list)))

(defn create-two-pair-score-list [freq-list]
  (->> (sort-by val > freq-list)
       move-higher-pair-to-front
       (map first)))

(defn create-pair-score-list [freq-list]
-  (as-> (sort-by val > freq-list) a
    (conj (sort-by key > (rest a)) (first a))
    (map first a)))

(defn order-three-kind-single-cards [freq-list]
  (if (> (last freq-list) (second freq-list))
    (concat (list (first freq-list))
            (list (last freq-list))
            (list (second freq-list)))
    freq-list))

(defn create-three-kind-score-list [freq-list]
  (->> (sort-by val > freq-list)
       (map first)
       order-three-kind-single-cards))

(defn create-value-list
  "Return a list with the <value of the hand> first, followed by the <values of the cards> in descending order (hand-value card-values...)"
  [hand]
  (let [hand-result    (determine-hand hand)
        card-freq-list (create-card-frequency-list hand)
        score          (hand-result hand-value-map)]
    (case hand-result
      :high-card      (conj (sort > (keys card-freq-list)) score)
      :pair           (conj (create-pair-score-list card-freq-list) score)
      :two-pair       (conj (create-two-pair-score-list card-freq-list) score)
      :three-kind     (conj (create-three-kind-score-list card-freq-list) score)
      :straight       (conj (sort > (keys card-freq-list)) score)
      :flush          (conj (sort > (keys card-freq-list)) score)
      :full-house     (conj (map first (sort-by val > card-freq-list)) score)
      :four-kind      (conj (map first (sort-by val > card-freq-list)) score)
      :straight-flush (conj (sort > (keys card-freq-list)) score)
      :royal-flush    (conj (sort > (keys card-freq-list)) score))))

(defn compare-card [card-1 card-2]
  (if (= card-1 card-2)
    0
    (if (> card-1 card-2)
           1
           2)))

(defn compare-hands [game]
  (loop [hand-1-value (create-value-list (take 5 game))
         hand-2-value (create-value-list (take-last 5 game))
]
    (if (not (= 0 (compare-card (first hand-1-value) (first hand-2-value))))
      (compare-card (first hand-1-value) (first hand-2-value))
      (recur (rest hand-1-value) (rest hand-2-value)))))

(defn score-players [game]
  (let [result  (compare-hands game)]
    (if (= result 1)
      (swap! player-one inc)
      (swap! player-two inc))))

(defn -main
  "Take in a file with list of 2 opposing poker hands. Scores the number of wins of each player"
  [& args]
  (println "Enter the filepath: ")
  (let [file-name (read-line)]
    (with-open [rdr (io/reader file-name)]
      (doseq [line (line-seq rdr)]
        (score-players (str/split line #" ")))))
  (println "Player 1: " @player-one)
  (println "Player 2: " @player-two))

:end-poker-scoring
