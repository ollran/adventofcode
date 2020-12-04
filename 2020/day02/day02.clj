(ns day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[a x] (-> line (str/split #": "))
        [b c] (-> a (str/split #" "))
        [d e] (-> b (str/split #"-"))]
    [(Integer/parseInt d) (Integer/parseInt e) (-> c char-array seq first) x]))

(defn get-input [name]
  (->> (io/reader name)
       slurp
       str/split-lines
       (map parse-line)))

(def test-input (get-input "test-input.txt"))

(def input (get-input "input.txt"))

(defn validate-password [password-vector]
  (let [[min-count max-count character password] password-vector
        char-appearance (get (frequencies (seq password)) character)]
    (cond (nil? char-appearance) false
          (< char-appearance min-count) false
          (> char-appearance max-count) false
          :else true)))

(defn validate-policy [password-vector]
  (let [[first-index second-index character password] password-vector
        password-char-vector (-> password char-array vec)
        first-appearance (get password-char-vector (dec first-index))
        second-appearance (get password-char-vector (dec second-index))]
    (cond (and (= first-appearance character) (not= second-appearance character)) true
          (and (not= first-appearance character) (= second-appearance character)) true
          :else false)))

(defn part01 [input]
  (->> input
       (map validate-password)
       (filter identity)
       count))

(defn part02 [input]
  (->> input
       (map validate-policy)
       (filter identity)
       count))

(println (part01 input))
(println (part02 input))

(comment
  (part01 test-input)
  (part02 test-input))
