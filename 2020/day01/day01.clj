(ns day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input [name]
  (->> (io/reader name)
       slurp
       str/split-lines
       (map #(Integer/parseInt %))
       vec))

(def test-input (get-input "test-input.txt"))

(def input (get-input "input.txt"))

(defn part01 [target input]
  (let [input-set (set input)]
    (->> input-set
         (filter #(contains? input-set (- target %)))
         (apply *))))

(defn part02 [target input]
  (first (for [x input
               :let [input-set (-> input set)
                     filtered-set (->> input-set
                                       (filter #(contains? input-set (- target x %)))
                                       set)]
               :when (->> filtered-set
                          (apply +)
                          (+ x)
                          (= target))]
           (->> filtered-set
                (apply *)
                (* x)))))

(println (part01 2020 input))
(println (part02 2020 input))

(comment
  (part01 2020 test-input)
  (part02 2020 test-input))
