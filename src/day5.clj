(ns day5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def data-file "day5.txt")

(def raw-data-parts
  (let [all (slurp (io/resource data-file))]
    (str/split all #"\n\n")))

(defn crate-prepped []
  (let [splitted (->> raw-data-parts
                      (first)
                      (str/split-lines))
        crates-only (take (dec (count splitted)) splitted)]
    crates-only))

(def instructions
  (->> raw-data-parts
       (second)
       (str/split-lines)
       (map #(re-matches #"move ([\d]+) from ([\d]+) to ([\d]+).*" %))
       (map #(drop 1 %))
       (map #(map read-string %))))

(defn get-row [crates n]
  (let [y (+ 1 (* 4 n))
        currs (map #(get % y) crates)
        reversed (reverse currs)
        filtered (take-while #(not= % \space) reversed)]
    (apply vector filtered)))

(defn get-rows [crates]
  (let [tuples
        (loop [so-far [] ind 0]
          (let [res (get-row crates ind)]
            (if (nil? (first res)) so-far
                (recur (conj so-far [(inc ind) res]) (inc ind)))))]
    (into (sorted-map) tuples)))

(defn move-one [rows from to]
  (let [from-items (get rows from)
        to-items (get rows to)
        from-top (peek from-items)
        rest-top (pop from-items)
        new-to (conj to-items from-top)
        new-map (assoc rows from rest-top)
        new-map (assoc new-map to new-to)]
    new-map))

(defn move [rows n from to]
  (loop [i n curr rows]
    (if (= i 0) curr
        (recur (dec i) (move-one curr from to)))))

(defn move-several [rows n from to]
  (let [from-items (get rows from)
        to-items (get rows to)
        from-top (reverse (take n (reverse from-items)))
        rest-top (reverse (drop n (reverse from-items)))
        new-to (concat to-items from-top)
        new-map (assoc rows from rest-top)
        new-map (assoc new-map to new-to)]
    new-map))

(defn q1 []
  (let [start-pos (get-rows (crate-prepped))
        end-res
        (reduce (fn [acc [n from to]]
                  (move acc n from to)) start-pos instructions)]
    (map peek (vals end-res))))

(defn q2 []
  (let [start-pos (get-rows (crate-prepped))
        end-res
        (reduce (fn [acc [n from to]]
                  (move-several acc n from to)) start-pos instructions)]
    (apply str (map last (vals end-res)))))
