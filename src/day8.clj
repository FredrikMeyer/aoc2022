(ns day8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def day8-test "30373
25512
65332
33549
35390")

(defn create-map [input]
  (let [by-lines (str/split-lines input)
        w (count (first by-lines))
        h (count by-lines)]
    {:height h :width w :points
     (into {}
           (for [i (range w)
                 j (range h)]
             [[i j] {:coord [i j] :height (read-string (str (get (get by-lines j) i)))}]))}))

;; direction is :row or :column
(defn get-subseq [mapp index direction reverse]
  (let [w (:width mapp)
        h (:height mapp)
        indices (if (= direction :row)
                  (for [i (if reverse (range (dec w) (dec 0) -1) (range w))]
                    [i index])
                  (for [i (if reverse (range (dec h) (dec 0) -1) (range h))] [index i]))
        points (:points mapp)
        vals (map (fn [ind] (get points ind)) indices)]
    vals))

(defn visible-coords [row]
  (loop [prev-var -1
         points row
         count #{}]
    (if (empty? points)
      count
      (let [curr-point (first points)
            height (:height curr-point)]
        (if (<= height prev-var)
          count
          (recur height (rest points) (conj count (:coord curr-point))))))))

(defn find-rows "Find all four directions form pt" [mapp coord]
  (let [[a b] coord
        [w h] [(:width mapp) (:height mapp)]
        right-indices (for [i (range (inc a) w)] [i b])
        left-indices (for [i (range (dec a) (dec 0) -1)] [i b])
        top-indices  (for [j (range (dec b) (dec 0) -1)] [a j])
        bottom-indices (for [j (range (inc b) h)] [a j])]
    (conj [] right-indices left-indices top-indices bottom-indices)))

(defn is-visible [mapp coord]
  (let [curr-height (:height (get (:points mapp) coord))
        neighbors (find-rows mapp coord)]
    (some identity (map (fn [n-heights] (every? identity (map #(< % curr-height) n-heights)))
                        (for [n neighbors]
                          (map (fn [c] (:height (get (:points mapp) c))) n))))))

(defn find-all-vis [mapp]
  (let [pts (:points mapp)
        coords (keys pts)]
    (filter #(is-visible mapp %) coords)))

(defn get-height [mapp coord]
  (:height (get (:points mapp) coord)))

(defn count-visble [mapp coord row]
  (let [curr-height (get-height mapp coord)]
    (loop [rrest row
           so-far 0]
      (if (empty? rrest) so-far
          (let [curr (first rrest)
                height (get-height mapp curr)]
            (if (>= height curr-height)
              (inc so-far)
              (recur (rest rrest) (inc so-far))))))))

(defn scenic-score-coord [mapp coord]
  (let [rows (find-rows mapp coord)]
    (reduce *
            (for [row rows]
              (count-visble mapp coord row)))))

(defn compute-scenic-scores [mapp]
  (let [coords (keys (:points mapp))]
    (map #(scenic-score-coord mapp %) coords)))

(defn q1 []
  (let [inp-raw (slurp (io/resource "day8.txt"))
        mapp (create-map inp-raw)
        all-vis (find-all-vis mapp)]
    (count all-vis)))

(defn q2 []
  (let [inp-raw (slurp (io/resource "day8.txt"))
        mapp (create-map inp-raw)
        scenic-scores (compute-scenic-scores mapp)]
    (reduce max scenic-scores)))
