(ns lab2.with-memo
  (:gen-class)
  (:require [lab2.common :as cmn]))

(defn integrate
  [f]
  (let [mem (atom {})]
    (letfn [(integrator [acc-sum b h]
              (if-let [e (find @mem [b h])]
                (val e)
                (if (< (- b h) 0)
                  acc-sum
                  (let [trapez (cmn/calculate-trapez f (- b h) b)
                        new-sum (+ acc-sum trapez)]
                    (swap! mem assoc [b h] new-sum)
                    (recur new-sum (- b h) h)))))

            (fancy-integrator [b h]
              (let [last-piece-length (rem b h)
                    last-piece-trapez (if (zero? last-piece-length)
                                        0
                                        (cmn/calculate-trapez f (- b last-piece-length) b))]
                (+ last-piece-trapez (integrator 0 (- b last-piece-length) h))))]

      fancy-integrator)))