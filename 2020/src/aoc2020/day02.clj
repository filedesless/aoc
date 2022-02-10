(ns aoc2020.day02)

(defn within? [lower upper value]
  (and (<= lower value) (>= upper value)))

(defn old-policy [min max letter password]
  "Tells wether password contains at least min times and a most max times letter"
  (->> password
       (filter (partial = letter))
       count
       (within? min max)))

(defn xor [p q]
  "Logical exclusive or"
  (or (and p (not q)) (and (not p) q)))

(defn new-policy [i j letter password]
  "Tells wether password has letter at index i and j (1-indexed)"
  (xor (= letter (get password (dec i))) (= letter (get password (dec j)))))

(def lines (clojure.string/split-lines (slurp "resources/02.txt")))

(defn parse [line]
  "'1-3 a: abcde' -> (1 3 a 'abcde')"
  (apply #(list (clojure.edn/read-string %1)
                (clojure.edn/read-string %2)
                (first %3) %4)
         (filter (partial not-empty)
                 (clojure.string/split line #"[- :]"))))

(defn day02 [policy]
  (count (filter #(apply policy (parse %)) lines)))

(defn day02a []
  (day02 old-policy))

(defn day02b []
  (day02 new-policy))
