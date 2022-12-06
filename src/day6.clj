(ns day6
  (:require
   [clojure.java.io :as io]))

(def data-file "day6.txt")

(def test "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def test "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def test "nppdvjthqldpwncqszvftbrmjlhg")
(def test "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
(def test "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")

(def real-data
  (slurp (io/resource "day6.txt")))

(defn find-first-occurence-of-difference [s n-distinct]
  (loop [ind 0]
    (let [current-window (subs s ind (+ ind n-distinct))]
      (if (= (count (set current-window)) n-distinct) (+ ind n-distinct)
          (recur (inc ind))))))

(defn q1 []
  (find-first-occurence-of-difference real-data 4))

(defn q2 []
  (find-first-occurence-of-difference real-data 14))
