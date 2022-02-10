(ns aoc2020.day03-test
  (:require [aoc2020.day03 :as sut]
            [clojure.test :as t]))

(def grid (sut/parse-file "dev-resources/03.txt"))

(t/deftest parse-file
  (t/is (= (first (first grid)) :open))
  (t/is (= (last (first grid)) :open))
  (t/is (= (first (last grid)) :open))
  (t/is (= (last (last grid)) :tree)))

(t/deftest toboggan
  (t/is (= '(:open :tree :open :tree :tree :open :tree :tree :tree :tree)
           (sut/toboggan grid 3 1))))

(t/deftest count-trees
  (let [f (partial apply sut/count-trees grid)]
    (t/is (= 2 (f (get sut/slopes 0))))
    (t/is (= 7 (f (get sut/slopes 1))))
    (t/is (= 3 (f (get sut/slopes 2))))
    (t/is (= 4 (f (get sut/slopes 3))))
    (t/is (= 2 (f (get sut/slopes 4))))))
