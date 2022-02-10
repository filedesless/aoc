(ns aoc2020.day01)

(def numbers
  (map clojure.edn/read-string
       (clojure.string/split-lines (slurp "resources/01.txt"))))

(defn day01 [entries]
  "Find the product of the entries whose sum is 2020"
  (->> entries
       (filter #(= 2020 (apply + %)))
       (map #(apply * %))
       first))

(def pairs (for [i numbers j numbers] [i j]))

(defn day01a []
  (day01 pairs))

(defn day01b []
  (day01 (for [i numbers pair pairs] (conj pair i))))


