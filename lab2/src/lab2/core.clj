(ns lab2.core
  (:gen-class)
  (:require
   [lab2.with-memo :as memo]
   [lab2.with-lazy :as lazy]))

;; Running examples
;; 
(defn f [x] (* (Math/sqrt x) x)) ; Integrable function
;; 
;; 1. With memoization
(def integral (memo/integrate f))
(integral 10 1/10)
;;
;; 2. With lazy sequence
(def h 1/10) ; Integration step. The lesser value produces better accurate, but uses more CPU and memory
(def seqq (lazy/integration-seq f h))
(lazy/get-integral-value f seqq h 10)

(defn -main
  [& args]
  (println "Hello world"))

(lab2.core/-main)