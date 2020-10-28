(ns lab2.core
  (:gen-class)
  (:require
   [lab2.with-memo :as memo]
   [lab2.with-lazy :as lazy]))

;; Running examples
;; 
(defn f [x] (* (Math/sqrt x) x)) ; Integrable function
(def h 1/10) ; Integration step. The lesser value produces better accurate, but uses more CPU and memory
;; 
;; 1. With memoization
(def integral-memo (memo/integrate f))
(integral-memo 10 1/10)
;; 
;; 2. With lazy sequence
(def integral-lazy (lazy/integrate f h))
(integral-lazy 10)

(defn -main
  [& args]
  (println "Hello world"))

(lab2.core/-main)