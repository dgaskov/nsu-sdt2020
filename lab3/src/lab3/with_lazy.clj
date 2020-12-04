(ns lab3.with-lazy
  (:gen-class)
  (:require [lab3.common :as cmn]))

(defn pfilter
  [f coll chunk-size batch-size]
  (let [chunks (cmn/partit chunk-size coll)
        lazy-filtered-chunks (map #(future (doall (filter f %))) chunks)
        batches (cmn/partit batch-size lazy-filtered-chunks)
        recursive-worker (fn recursive-worker [futures]
                                (lazy-seq
                                 (when-let [first-future-batch (first futures)]
                                   (let [fair-calculated (mapcat deref (doall first-future-batch))]
                                     (cons fair-calculated (recursive-worker (rest futures)))))))]
    (mapcat identity (recursive-worker batches))))