(ns day9
  (:require [clojure.java.io :as io]
            [clojure.core.match :as m]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn test-data [file-name]
  (->> (slurp (io/resource file-name))
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [[d n]] [d (read-string n)]))))

(defn tail-touching? [pos tail]
  (let [[x y] pos
        [a b] tail
        x-diff (abs (- a x))
        y-diff (abs (- b y))]
    (< (max x-diff y-diff) 2)))

(defn unitize [[a b]]
  [(if (zero? a) 0 (/ a (abs a)))
   (if (zero? b) 0 (/ b (abs b)))])

(defn same-row-or-column? [p q]
  (let [[x y] p
        [a b] q]
    (or (= x a) (= y b))))

(defn move-to-same-row-or-column [state]
  (let [head (:pos state)
        tail (:tail state)
        [a b] head
        [x y] tail
        diff-v [(- a x) (- b y)]
        [dx dy] (unitize diff-v)
        new-tail-pos     [(+ x dx) (+ y dy)]]
    (-> state
        (assoc :tail new-tail-pos)
        (update :tail-coords #(conj % new-tail-pos)))))

(defn move-close-enough "Assumes they are in same/row column" [state]
  (loop [curr-state state]
    (let [tail (:tail curr-state)
          curr-pos (:pos curr-state)]
      (if (tail-touching? curr-pos tail) curr-state
          (recur (move-to-same-row-or-column curr-state))))))

;; Initial pos {:pos [0 0] :tail [0 0] :tail-coords #{[0 0]}}
(defn step [state instruction]
  (let [[dir n] instruction
        [x y] (:pos state)
        updated-pos (m/match [dir]
                      ["U"] (-> state
                                (assoc :pos [x (+ y n)]))
                      ["D"] (assoc state :pos [x (- y n)])
                      ["L"] (assoc state :pos [(- x n) y])
                      ["R"] (assoc state :pos [(+ x n) y]))
        new-tail-pos (move-close-enough updated-pos)]
    new-tail-pos))

(defn do-steps [start-state instructions]
  (reduce (fn [acc curr] (step acc curr)) start-state instructions))

(defn q1 []
  (let [data (test-data "day9.txt")
        start-state {:pos [0 0] :tail [0 0] :tail-coords #{[0 0]}}
        computed-state (do-steps start-state data)]
    computed-state
    ))
