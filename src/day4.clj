(ns day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [common :as c]))

(def data (slurp (io/resource "day4.txt")))

(def data-prepped
  (->> data
       (str/split-lines)
       (map (fn [d] (rest (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" d))))
       (map (fn [d] (map read-string d)))))

(defn fully-contains [pair]
  (let [[a b c d] pair]
    (or
     (and (<= a c) (>= b d))
     (and (<= c a) (>= d b)))))

(defn overlap [pair]
  (let [[a b c d] pair]
    (if (<= d b)
      (<= a d)
      (<= c b))))

(defn q1 []
  (->> data-prepped
       (map fully-contains)
       (map (fn [b] (if b 1 0)))
       (c/sum-seq)))

(defn q2 []
  (->> data-prepped
       (map overlap)
       (map (fn [b] (if b 1 0)))
       (c/sum-seq)))
