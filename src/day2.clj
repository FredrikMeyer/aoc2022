(ns day2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [common :as c]))

(def data (slurp (io/resource "day2.txt")))

(def translation {"A" :rock "B" :paper "C" :scissors "X" :rock "Y" :paper "Z" :scissors})

(def inp (->> data
              (str/split-lines)
              (map (fn [d] (str/split d #" ")))
              (map (fn [d] (map (fn [e] (get translation e)) d)))))

(def values {:rock 1 :paper 2 :scissors 3})

(def res {:rock {:rock 3 :paper 0 :scissors 6}
          :paper {:rock 6 :paper 3 :scissors 0}
          :scissors {:rock 0 :paper 6 :scissors 3}})

(def inptest [[:rock :paper] [:paper :rock] [:scissors :scissors]])

(defn q1 []
  (c/sum-seq (->> inp
                  (map (fn [[opp you]]
                         (+ (get-in res [you opp]) (get values you)))))))
