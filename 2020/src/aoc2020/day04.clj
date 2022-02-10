(ns aoc2020.day04)

(def required_fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defn parse "parse input file into a list of associative maps" [filename]
  (let [passeports (clojure.string/split (slurp filename) #"\n\n")]
    (for [passeport passeports]
      (apply hash-map (clojure.string/split passeport #"[\s:]")))))

(defn valid? "checks that a passeport contains all the require fields" [passeport]
  (every? (fn [field] (contains? passeport field)) required_fields))

(defn day04a []
  (count (filter valid? (parse "resources/04.txt"))))

(defn byr_valid? [byr]
  (<= 1920 (Integer/parseInt byr) 2002))

(defn iyr_valid? [iyr]
  (<= 2010 (Integer/parseInt iyr) 2020))

(defn eyr_valid? [eyr]
  (<= 2020 (Integer/parseInt eyr) 2030))

(defn hgt_valid? [hgt]
  (case  ))

(defn hcl_valid? [hcl]
  true)

(defn pid_valid? [pid]
  true)
