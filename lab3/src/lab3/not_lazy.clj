(ns lab3.not-lazy
  (:gen-class)
  (:require [lab3.common :as cmn]))

(defn pfilter
  [f coll block-size]
  (->>
   coll
   (cmn/partit block-size)
   (map #(future (doall (filter f %))))
  ;  (cmn/spy) ; For debugging only
   (doall)
   (mapcat deref)
   (doall)))