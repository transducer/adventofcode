(defproject adventofcode "1.0.0"
  :description "Advent of Code 2016"
  :url "https://adventofcode.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [digest "1.4.5"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [com.taoensso/timbre "4.8.0"]
                 [org.clojure/data.priority-map "0.0.7"]]
  :main ^:skip-aot adventofcode.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
