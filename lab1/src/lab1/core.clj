(ns lab1.core
  (:gen-class))

(defn my-map
  [f coll]
  (reduce (fn [acc x]
            (conj acc (f x)))
          []
          coll))

(defn my-filter
  [f coll]
  (reduce #(if (f %2)
             (conj %1 %2)
             %1)
          []
          coll))

(defn my-mapcat
  ([f & colls]
   (reduce (fn [acc x] (concat acc (f x)))
           []
           colls)))

(defn shift
  [coll]
  (concat (next coll) (list (first coll))))


(defn permutation
  [max-length symbols]
  (letfn [(recursive-worker [generated-permutations
                             shiftable-symbols
                             current-permutation]

            (if (and (empty? current-permutation) (= symbols shiftable-symbols))
              generated-permutations

              ;; else
              (if (= (count current-permutation) max-length)
                (recur (conj generated-permutations current-permutation)
                       shiftable-symbols
                       (butlast current-permutation))

                ;; else
                (if (not (= (last current-permutation) (first shiftable-symbols)))
                  (recur generated-permutations
                         (shift shiftable-symbols)
                         (concat current-permutation (list (first shiftable-symbols))))

                  ;; else
                  (recur generated-permutations
                         (shift shiftable-symbols)
                         (butlast current-permutation))))))

          (runner [] (recursive-worker '()
                                       (shift symbols)
                                       (list (first symbols))))]
    (reverse (runner))))

;; Permutation using map/reduce/filter (and mapcat)

(defn permutation-2
  [max-length all-symbols]
  (reduce (fn [acc _]
            (mapcat (fn [partial-permutation]
                      (let [allowed-symbols (filter (fn [sym] (not= (last partial-permutation) sym))
                                                    all-symbols)]
                        (map (fn [allowed-symbol] (concat partial-permutation
                                                          (list allowed-symbol)))
                             allowed-symbols)))
                    acc))
          (map list all-symbols)
          (repeat (dec max-length) 1)))

(defn -main
  [& args]
  (if (>= (count args) 2)
    (let [max-length (first args)
          max-length (Integer/parseInt max-length)
          uniquie-sumbols (distinct (next args))]
      (println (permutation max-length uniquie-sumbols)))
    (println "Bad number of args. Expected 2 or more: N and collection")))

(lab1.core/-main "2" "a" "b" "c")