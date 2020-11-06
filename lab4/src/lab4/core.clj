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

(def expr (alcore/make-dnf e))

(def ex1 (alcore/signify-variable-and-make-dnf expr a false))
(def ex2 (alcore/signify-variable-and-make-dnf ex1 b true))
(def ex3 (alcore/signify-variable-and-make-dnf ex2 c true))
(def ex4 (alcore/signify-variable-and-make-dnf ex3 d true))

(alcore/simplify ex4)

(try
  (alcore/simplify ex4)
  (catch Exception exc
    (println (ex-message exc) (ex-data exc))))