(ns lab3.core
  (:gen-class)
  (:require [lab3.common :as cmn]
            [lab3.not-lazy :as not-lazy]
            [lab3.with-lazy :as with-lazy]))

;; Run examples

;; Not-lazy filter, with comparance to standart filter
;; (time (println (not-lazy/pfilter cmn/heavy-cond (range 12) 1))) ; "Elapsed time: 509.112305 msecs"
;; (time (println (filter cmn/heavy-cond (range 12)))) ; "Elapsed time: 6014.882469 msecs"

;; Lazy parallel filter
;; (time (println (take 5 (with-lazy/pfilter cmn/heavy-cond (range) 1 3))))

;; Standart filter
;; (time (println (take 5 (filter cmn/heavy-cond (range)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))