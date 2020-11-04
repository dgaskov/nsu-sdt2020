(ns lab4.core
  (:gen-class)
  (:require [lab4.bool-algebra-core :as alcore]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def a (alcore/variable :a))
(def b (alcore/variable :b))
(def c (alcore/variable :c))
(def d (alcore/variable :d))

;; ((a & !b) | c) -> (d | !(b -> c))
;; !((a & !b) | c) v (d | !(!b v c))
;; (!(a & !b) & !c) | (d | (b & !c))
;; ((!a | b) & !c) | (d | (b & !c))
;; ( (!a & !c) | (b & !c) ) | (d | (b & !c))

(def e (alcore/implication (alcore/disjunction (alcore/conjunction a (alcore/negation b))
                                               c)
                           (alcore/disjunction d
                                               (alcore/negation (alcore/implication b c)))
                           ))

(try
  (alcore/make-dnf e)
  (catch Exception exc
    (println (ex-message exc) (ex-data exc))))


(def x (alcore/variable :x))
(def y (alcore/variable :y))
(def z (alcore/variable :z))

(def expr (alcore/conjunction x
                              (alcore/disjunction y z)))

(alcore/apply-distribution-rules expr)

(def condd (fn [expr] (or (alcore/constant? expr)
                         (alcore/variable? expr)

                         (and (alcore/negation expr)
                              
                              (if-let [[arg] (alcore/args expr)]
                                (or (alcore/constant? arg)
                                    (alcore/variable? arg))
                                false)))))

(condd expr)

(alcore/negation? expr)