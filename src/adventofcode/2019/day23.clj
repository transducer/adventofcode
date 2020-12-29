(ns adventofcode.2019.day23
  (:require
   [adventofcode.2019.intcode :refer [run-async]]
   [clojure.core.async :as async :refer [chan alts!! <!! >!! timeout sliding-buffer]]))

(def program
  (slurp "resources/2019/day23.txt"))

(def n-computers 50)

(defn boot! [network-address in out]
  (run-async program in out)
  (>!! in network-address))

(def network
  (into {}
        (for [network-address (range n-computers)
              :let [in (chan 15)
                    out (chan 15)
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

(def address-zero-previous-y (atom nil))
(def nat-address 255)
(def nat (chan (sliding-buffer 2)))

(loop [sending-address 0, idle-count 0, idle-reset? false]
  (let [[in out] (network sending-address)
        _ (when-not idle-reset? (>!! in -1))
        [receiving-address _] (alts!! [(timeout 0) out])]
    (if-not receiving-address
      (if (= idle-count (* 2 n-computers))
        (let [[in _] (network 0)
              x (<!! nat)
              y (<!! nat)]
          (>!! in -1)
          (>!! in x)
          (>!! in y)
          (recur 1 0 true))
        (recur (mod (inc sending-address) (dec n-computers)) (inc idle-count) false))
      (let [[in _] (network receiving-address)
            x (<!! out)
            y (<!! out)]
        (cond (= receiving-address nat-address)
              (do (prn y)
                  (>!! nat x)
                  (>!! nat y))
              (zero? receiving-address)
              (do (reset! address-zero-previous-y y)
                  (>!! in -1)
                  (>!! in x)
                  (>!! in y))
              :else
              (do (>!! in -1)
                  (>!! in x)
                  (>!! in y)))
        (recur (mod (inc sending-address) (dec n-computers)) 0 false)))))
;; => 15210
