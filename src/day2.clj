(ns day2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [common :as c]))

(def data (slurp (io/resource "day2.txt")))

(def translation-1 {"A" :rock "B" :paper "C" :scissors "X" :rock "Y" :paper "Z" :scissors})
(def translation-2 {"A" :rock "B" :paper "C" :scissors "X" :win "Y" :draw "Z" :lose})

(def inp (->> data
              (str/split-lines)
              (map (fn [d] (str/split d #" ")))))

(def values {:rock 1 :paper 2 :scissors 3})

(def res {:rock {:rock 3 :paper 0 :scissors 6}
          :paper {:rock 6 :paper 3 :scissors 0}
          :scissors {:rock 0 :paper 6 :scissors 3}})

(defn q1 []
  (c/sum-seq (->> inp
                  (map (fn [d] (map (fn [e] (get translation-1 e)) d)))
                  (map (fn [[opp you]]
                         (+ (get-in res [you opp]) (get values you)))))))

(def how-to-draw
  (let [val->res {0 :lose 3 :draw 6 :win}]
    (into {} (for [tool [:rock :paper :scissors]]
               [tool (into {} (for [[k v] (get res tool)] [(get val->res v) k]))]))))

(defn q2 []
  (c/sum-seq
   (->> inp
        (map (fn [d] (map (fn [e] (get translation-2 e)) d)))
        (map (fn [[opp r]] [opp (get-in how-to-draw [opp r])]))
        (map (fn [[opp you]]
               (+ (get-in res [you opp]) (get values you)))))))
