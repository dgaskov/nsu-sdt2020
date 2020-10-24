(ns lab3.with-lazy
  (:gen-class)
  (:require [lab3.common :as cmn]))

; NOT READY YET
;; (defn pfilter
;;   [f coll block-size]
;;   (let [partitioned (partit block-size coll)
;;         lazy-filtered (map #(future (doall (filter f %))) partitioned)
;;         step (fn step [[x & xs :as vs] fs]
;;                (lazy-seq
;;                 (if-let [s (seq fs)]
;;                   (cons (deref x) (step xs (rest s)))
;;                   (map deref vs))))
;;         runner (step lazy-filtered (drop block-size lazy-filtered))]
;;     (mapcat identity runner)))

;; (defn pfilter
;;   [f coll]
;;   (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
;;         rets (map #(future (if (f %) % nil)) coll)
;;         step (fn step [[x & xs :as vs] fs]
;;                (println "x=" x "xs=" xs "vs=" vs "fs=" fs)
;;                (lazy-seq
;;                 (if-let [s (seq fs)]
;;                   (cons (deref x) (step xs (rest s)))
;;                   (map deref vs))))]
;;     (step rets (drop n rets))))