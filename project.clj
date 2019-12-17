(defproject adventofcode "1.0.0"
  :description "Advent of Code"
  :url "https://adventofcode.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [digest "1.4.5"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [com.taoensso/timbre "4.8.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [fast-zip "0.7.0"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [org.clojure/test.check "0.10.0"]
                 [org.clojure/core.async "0.6.532"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [quil "3.1.0"]]
  :main ^:skip-aot adventofcode.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
