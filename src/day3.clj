(ns day3
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as s]
   [common :as c]))

(def data (slurp (io/resource "day3.txt")))

(def data-prepped
  (->> data
       (str/split-lines)))

(defn is-upper-case [c]
  (and (<= (int c) 90) (>= (int c) 65)))

(defn char->val [c]
  (if (is-upper-case c)
    (- (int c) 38)
    (- (int c) 96)))

(defn find-common [el]
  (let [n (count el)
        h (/ n 2)
        f (set (take h el))
        l (set (drop h el))]
    (s/intersection f l)))

(defn q1 []
  (c/sum-seq (->> data-prepped
                  (map find-common)
                  (map first)
                  (map char->val))))

(defn find-common-letter [data]
  (apply s/intersection (map set data)))

(defn q2 []
  (let [partitioned (partition 3 data-prepped)]
    (c/sum-seq (->> partitioned
                    (map find-common-letter)
                    (map first)
                    (map char->val)))))
