(defproject adventofcode "1.0.0"
  :description "Advent of Code"
  :url "https://adventofcode.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [digest "1.4.9"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [com.taoensso/timbre "5.1.0"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [fast-zip "0.7.0"]
                 [org.clojure/spec.alpha "0.2.187"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/core.async "1.3.610"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [quil "3.1.0"]
                 [medley "1.3.0"]]
  :main ^:skip-aot adventofcode.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
