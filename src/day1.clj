(ns day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def data (slurp (io/resource "day1.txt")))

(defn sum-seq [seq]
  (reduce + seq))

(def sums
  (->>
   (str/split data #"\n\n")
   (map (fn [d] (map read-string (str/split-lines d))))
   (map sum-seq)))

(def q1 (apply max sums))

(def q2
  (sum-seq (->> sums
                (sort >)
                (take 3))))

(defn hello [args]
  (println q1 q2 args))
