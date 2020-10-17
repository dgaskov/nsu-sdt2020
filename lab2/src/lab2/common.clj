(ns lab2.common
  (:gen-class))

(defn calculate-trapez
  "Calculates trapezioid's square between points [xi, xi-1, f(xi-1), f(xi)].
   Uses `h` as an integration step - distance between xi and xi-1"
  [f xi h]
  (let [xi-1 (- xi h)
        sy (+ (f xi-1) (f xi))
        trapez (* (/ sy 2) (- xi xi-1))]
    (println "Calculated backward for xi =" xi "h =" h ":" trapez)
    trapez))