(ns aoc2020.day04-test
  (:require [aoc2020.day04 :as sut]
            [clojure.test :as t]))

(def passeports
  '({"hgt" "183cm",
     "pid" "860033327",
     "byr" "1937",
     "eyr" "2020",
     "iyr" "2017",
     "ecl" "gry",
     "cid" "147",
     "hcl" "#fffffd"}
    {"pid" "028048884",
     "byr" "1929",
     "eyr" "2023",
     "iyr" "2013",
     "ecl" "amb",
     "cid" "350",
     "hcl" "#cfa07d"}
    {"hgt" "179cm",
     "pid" "760753108",
     "byr" "1931",
     "eyr" "2024",
     "iyr" "2013",
     "ecl" "brn",
     "hcl" "#ae17e1"}
    {"hgt" "59in",
     "pid" "166559648",
     "eyr" "2025",
     "iyr" "2011",
     "ecl" "brn",
     "hcl" "#cfa07d"}))

(t/deftest parse
  (t/is (= (sut/parse "dev-resources/04.txt") passeports)))

(t/deftest valid?
  (t/is (sut/valid? (nth passeports 0)))
  (t/is (not (sut/valid? (nth passeports 1))))
  (t/is (sut/valid? (nth passeports 2)))
  (t/is (not (sut/valid? (nth passeports 3)))))

(t/deftest byr_valid?
  (t/is (sut/byr_valid? "2002"))
  (t/is (not (sut/byr_valid? "2003"))))

(t/deftest hgt_valid?
  (t/is (sut/hgt_valid? "60in"))
  (t/is (sut/hgt_valid? "190cm"))
  (t/is (not (sut/hgt_valid? "190in")))
  (t/is (not (sut/hgt_valid? "190"))))

(t/deftest hcl_valid?
  (t/is (sut/hcl_valid? "#123abc"))
  (t/is (not (sut/hcl_valid? "#123abz")))
  (t/is (not (sut/hcl_valid? "123abc"))))

(t/deftest pid_valid?
  (t/is (sut/pid_valid "000000001"))
  (t/is (not (sut/pid_valid "0123456789"))))
