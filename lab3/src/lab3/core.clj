(ns lab3.core
  (:gen-class))

(defn heavy-cond
  [x]
  (Thread/sleep 1000)
  (even? x))

(def spy #(do (println "DEBUG:" %) %))

(defn partit
  ([n coll]
   (lazy-seq
    (when-let [sequence (seq coll)] ; if coll is not empty
      (cons (take n sequence) ; Return cons of new block and new partition from rest
            (partit n (nthrest sequence n)))))))

(defn pfilter
  [f coll block-size]
  (->>
   coll
   (partit block-size)
   (map #(future (doall (filter f %))))
   (spy)
   (map deref)
   (mapcat identity)
   (doall)))

(time (println (pfilter heavy-cond (range 10) 2)))
;; (time (println (take 10 (pfilter heavy-cond (range) 2)))) Lazy example


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))