(ns lab2.with-lazy
  (:gen-class)
  (:require [lab2.common :as cmn]))

(defn- integration-seq
  [f h]
  (letfn [(integrator [acc-sum xi h]
            (lazy-seq (let [trapez (cmn/calculate-trapez f (- xi h) xi)
                            new-sum (+ acc-sum trapez)]
                        (cons acc-sum (integrator new-sum (+ xi h) h)))))]
    (integrator 0 h h)))

(defn- get-integral-value
  [f seq h b]
  (let [whole-pieces-n (quot b h)
        last-piece-length (rem b h)
        last-piece-trapez (if (zero? last-piece-length)
                            0
                            (cmn/calculate-trapez f (- b last-piece-length) b))]
    (+ last-piece-trapez (nth seq whole-pieces-n))))

(defn integrate
  [f h]
  (let [integration-seq (integration-seq f h)]
    #(get-integral-value f integration-seq h %)))