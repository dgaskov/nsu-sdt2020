(ns lab2.with-lazy
  (:gen-class)
  (:require [lab2.common :as common]))

(defn integration-seq
  [f h]
  (letfn [(integrator [acc-sum xi h]
            (lazy-seq (let [trapez (common/calculate-trapez f xi h)
                            new-sum (+ acc-sum trapez)]
                        (cons acc-sum (integrator new-sum (+ xi h) h)))))]
    (integrator 0 h h)))

(defn get-integral-value
  [f seq h b]
  (let [whole-pieces-n (quot b h)
        last-piece-length (rem b h)
        last-piece-trapez (if (= last-piece-length 0)
                            0
                            (common/calculate-trapez f b last-piece-length))]
    (+ last-piece-trapez (nth seq whole-pieces-n))))