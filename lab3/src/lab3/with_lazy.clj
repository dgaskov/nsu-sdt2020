(ns lab3.with-lazy
  (:gen-class)
  (:require [lab3.common :as cmn]))

(defn pfilter
  [f coll block-size batch-size]
  (let [partitioned (cmn/partit block-size coll)
        lazy-filtered (map #(future (doall (filter f %))) partitioned)
        recursive-worker (fn recursive-worker [fs]
                           (lazy-seq
                            (when-let [batch (seq (take batch-size fs))]
                              (let [fair-calculated (mapcat deref (doall batch))]
                                (cons fair-calculated (recursive-worker (drop batch-size fs)))))))]
    (mapcat identity (recursive-worker lazy-filtered))))