(ns lab3.core
  (:gen-class)
  (:require [lab3.common :as cmn]
            [lab3.not-lazy :as not-lazy]
            [lab3.with-lazy :as with-lazy]))

; Run examples
; (time (println (not-lazy/pfilter cmn/heavy-cond (range 12) 1))) ; "Elapsed time: 509.112305 msecs"
; (time (println (filter cmn/heavy-cond (range 12)))) ; "Elapsed time: 6014.882469 msecs"


;; Lazy version is not ready yet
;; (time (println (take 6 (pfilter-lazy heavy-cond (range) 1)))) ; Lazy example

;; (time (println (take 10 (filter heavy-cond (range)))))
;; (time (println (take 12 (pfil heavy-cond (range)))))
;; (time (println (take 10 (pmap heavy-inc (range)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))