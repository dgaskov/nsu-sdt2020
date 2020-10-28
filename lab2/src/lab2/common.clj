(ns lab2.common
  (:gen-class))

(defn calculate-trapez
  "Calculates trapezioid's square between points [a, b, f(a), f(b)].
   Value of 'a' must be less or equal to 'b': 'a <= b'."
  [f a b]
  (let [sy (+ (f a) (f b))
        trapez (* (/ sy 2) (- b a))]
    (println "Calculated backward for a =" a "b =" b ":" trapez)
    trapez))