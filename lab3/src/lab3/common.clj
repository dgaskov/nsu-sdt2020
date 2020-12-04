(ns lab3.common
  (:gen-class))

(defn heavy-cond
  [x]
  (Thread/sleep 500)
  (even? x))

(defn heavy-inc
  [x]
  (Thread/sleep 500)
  (inc x))

(def spy #(do (println "DEBUG:" %) %))

(defn partit
  ([n coll]
   (lazy-seq
    (when-let [sequence (seq coll)] ; if coll is not empty
      (cons (take n sequence) ; Return cons of new block and new partition from rest
            (partit n (drop n sequence)))))))