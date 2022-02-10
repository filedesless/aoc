(ns aoc2020.core
  (:gen-class)
  (:require [aoc2020.day01 :as day01])
  (:require [aoc2020.day02 :as day02])
  (:require [aoc2020.day03 :as day03])
  (:require [aoc2020.day04 :as day04])
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Day01a:" (day01/day01a))
  (println "Day01b:" (day01/day01b))
  (println "Day02b:" (day02/day02b))
  (println "Day02b:" (day02/day02b))
  (println "Day03a:" (day03/day03a))
  (println "Day03b:" (day03/day03b))
  (println "Day04a:" (day04/day04a))
  )
