(ns aoc2020.day02-test
  (:require [aoc2020.day02 :as sut]
            [clojure.test :as t]))

(t/deftest old-policy
  (t/is (sut/old-policy 1 3 \a "abcde"))
  (t/is (not (sut/old-policy 1 3 \b "cdefg")))
  (t/is (sut/old-policy 2 9 \c "ccccccccc")))

(t/deftest new-policy
  (t/is (sut/new-policy 1 3 \a "abcde"))
  (t/is (not (sut/new-policy 1 3 \b "cdefg")))
  (t/is (not (sut/new-policy 2 9 \c "ccccccccc"))))
