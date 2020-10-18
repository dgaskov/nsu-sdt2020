(ns lab3.core
  (:gen-class))

(defn heavy-cond
  [x]
  (Thread/sleep 100)
  (even? x))

(defn heavy-inc
  [x]
  (Thread/sleep 100)
  (inc x))

(def spy #(do (println "DEBUG:" %) %))

(defn partit
  ([n coll]
   (lazy-seq
    (when-let [sequence (seq coll)] ; if coll is not empty
      (cons (take n sequence) ; Return cons of new block and new partition from rest
            (partit n (drop n sequence)))))))

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

(defn pfilter-lazy
  [f coll block-size]
  (let [partitioned (partit block-size coll)
        lazy-filtered (map #(future (filter f %)) partitioned)
        step (fn step [[x & xs :as vs] fs]
               (lazy-seq
                (if-let [s (seq fs)]
                  (cons (deref x) (step xs (rest s)))
                  (map deref vs))))
        runner (step lazy-filtered (drop block-size lazy-filtered))]
    (mapcat identity runner))
  )

(time (println (pfilter heavy-cond (range 10) 2)))
(time (println (take 10
                     (pfilter-lazy heavy-cond (range) 10)))) ; Lazy example

(time (println (take 10 (filter heavy-cond (range)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))