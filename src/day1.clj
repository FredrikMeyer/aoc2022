(ns day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [common :as c]))

(def data (slurp (io/resource "day1.txt")))

(def sums
  (->>
   (str/split data #"\n\n")
   (map (fn [d] (map read-string (str/split-lines d))))
   (map c/sum-seq)))

(def q1 (apply max sums))

(def q2
  (c/sum-seq (->> sums
                (sort >)
                (take 3))))

(defn hello [args]
  (println q1 q2 args))
