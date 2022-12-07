(ns day7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]
   [common :as c]
   [clojure.core.match :as m]
   [clojure.core.match.regex]))

(def test-data (slurp (io/resource "day7.txt")))

(defn parse-input
  "Create list of tokens."
  [data]
  (let [lines (str/split-lines data)]
    (for [l lines]
      (let [[f s t] (str/split l #" ")]
        (m/match [f s t]
          ["$" "cd" _] {:type :cd :value t}
          ["$" "ls" _] {:type :ls}
          [#"\d+" _ _] {:type :file :value s :size (read-string f)}
          ["dir" _ _] {:type :directory :value s}
          :else {:type :unknown})))))

;; Signature: state -> state
;; Where state loks like this { :current-dir ["/" "b"] :input [<list of commands>] }
(defn consume [state]
  (let [cmd (first (:input state))
        rest-input (rest (:input state))
        current-dir (:current-dir state)]
    (case (:type cmd)
      :cd (-> state
              (update :current-dir #(if (= (:value cmd) "..") (pop %) (conj % (:value cmd))))
              (assoc :input rest-input))
      :ls (let [[files-and-dirs new-input] (split-with (fn [i]
                                                         (or (= (:type i) :file)
                                                             (= (:type i) :directory))) rest-input)
                files (filter (fn [f] (= (:type f) :file)) files-and-dirs)
                files-mapped (map (fn [f] {:size (:size f) :name (:value f)}) files)]
            (-> state
                (update-in current-dir (fn [dir]
                                         (-> dir
                                             (assoc :path current-dir)
                                             (update :files #(set/union % (set files-mapped))))))
                (assoc :input new-input))))))

(defn build-tree [input]
  (loop [state {:current-dir [] :input input}]
    (if (= (count (:input state)) 0)
      state
      (recur (consume state)))))

(defn sum-files-no-dirs [dir]
  (let [file-size (let [files (:files dir)]
                    (->> files
                         (map :size)
                         c/sum-seq))]
    file-size))

;; Do a recursive depth first traversal of the directory tree
;; to build up :dir-size on all nodes.
(defn map-tree [limit dir-comparator]
  (let [tree (build-tree (parse-input test-data))]
    (loop [queue [tree]
           acc tree ;; Goal is to work through whole tree
           found-directories []]
      (let [curr (last queue)
            rest-queue (try (pop queue) (catch Exception _ (vector)))
            path (:path curr)
            sub-dirs (apply vector (filter #(not (keyword? %)) (keys curr)))
            sub-dirs-mapped (map #(get-in acc (conj path %)) sub-dirs)
            not-visited-sub-dirs (filter (fn [d] (= (:dir-size d) nil)) sub-dirs-mapped)]

        (if (nil? curr) [acc found-directories]
            (cond (= (count sub-dirs) 0)
                  (let [dir-size (sum-files-no-dirs curr)
                        updated-dirs (if (dir-comparator dir-size limit) (conj found-directories (:path curr)) found-directories)]

                    (recur rest-queue
                           (update-in acc (:path curr)
                                      (fn [old] (assoc old :dir-size (sum-files-no-dirs old))))
                           updated-dirs))
                  (= (count not-visited-sub-dirs) 0)
                  (let [dir-size (+ (reduce + (map :dir-size sub-dirs-mapped))
                                    (sum-files-no-dirs curr))
                        updated-dirs (if (dir-comparator dir-size limit) (conj found-directories (:path curr)) found-directories)]
                    (recur rest-queue
                           (update-in acc (:path curr)
                                      (fn [old] (assoc old :dir-size dir-size)))
                           updated-dirs))
                  :else
                  (let [new-queue (into queue not-visited-sub-dirs)]
                    (recur new-queue acc found-directories))))))))

(defn q1 []
  (let [[mapped found-dirs] (map-tree 100000 <=)
        ddd (map (fn [p] (get-in mapped p)) found-dirs)
        eee (map :dir-size ddd)]
    (c/sum-seq eee)))

(defn get-total-size [mapped-tree]
  (:dir-size (get mapped-tree "/")))

(defn q2 []
  ;; (- 30000000 (- 70000000 (get-total-size (first (map-tree 0 <=))))) ;; free space
  ;; > 7442399
  ;; (:dir-size (get-in (first (map-tree 7442399 >=)) ["/" "rvstq" "wrmm" "nlwpspl" "bmmmhnbc"]))
  ;; Just choose the deepest directory
  )
