(ns adventofcode.2019.day23
  (:require [adventofcode.2019.intcode :refer [run-async]]
            [clojure.core.async :as async :refer [chan alts!! <!! >!! timeout]]))

(def program
  (slurp "resources/2019/day23.txt"))


;; Part 1

(def n-computers 50)

(defn boot! [network-address in out]
  (run-async program in out)
  (>!! in network-address))

(def network
  (into {}
        (for [network-address (range n-computers)
              :let [in (chan 10)
                    out (chan 10)
                    _ (boot! network-address in out)]]
          [network-address [in out]])))

(loop [sending-address 0]
  (let [[in out] (network sending-address)
        _ (>!! in -1)
        [receiving-address _] (alts!! [(timeout 0) out])]
    (if receiving-address
      (let [[in _] (network receiving-address)
            x (<!! out)
            y (<!! out)]
        (if (= receiving-address 255)
          y
          (do (>!! in -1)
              (>!! in x)
              (>!! in y)
              (recur (mod (inc sending-address) (dec n-computers))))))
      (recur (mod (inc sending-address) (dec n-computers))))))

;; => 22877
