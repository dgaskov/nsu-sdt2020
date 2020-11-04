(ns lab4.bool-algebra-core
  (:gen-class))



;; CONSTANTS

(defn constant [value]
  {:pre [(boolean? value)]}
  (list ::const value))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [const]
  (second const))



;; VARIABLES

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [var]
  (second var))

(defn same-variables? [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= (variable-name v1)
      (variable-name v2))))



;; OPERATORS

(defn conjunction [expr1 expr2]
  (cons ::conj (cons expr1 (list expr2))))

(defn conjunction? [expr]
  (= ::conj (first expr)))


(defn disjunction [expr1 expr2]
  (cons ::disj (cons expr1 (list expr2))))

(defn disjunction? [expr]
  (= ::disj (first expr)))


(defn negation [expr]
  (cons ::neg (list expr)))

(defn negation? [expr]
  (= ::neg (first expr)))


(defn implication [expr1 expr2]
  (cons ::impl (cons expr1 (list expr2))))

(defn implication? [expr]
  (= ::impl (first expr)))



;; COMMON UTILS

(defn args [expr]
  (rest expr))

(defn apply-translation-table
  "Applies given translation table to the expression.
   Translation table is a list of `rules` - pairs of two values.
   First is a `predicate`, which defines should we apply
   this rule to the expression, or not (must return bool).
   Second is a `transform` - function which takes an expression
   with optional arguments, and returns new modified expression"
  [expr translation-table & args]
  (if-some [[transform
             rule-index] (some (fn [[idx rule]]
                                 (if ((first rule) expr)
                                   [(second rule) idx]
                                   false))
                               (map-indexed list translation-table))]
    (do
      ;; (println "Apply rule with index" rule-index "and args" args)
      (transform expr args))
    (throw (ex-info
            "Could not found any rules for given expression"
            {:expresion expr
             :number-of-rules (count translation-table)}))))



;; INTERNAL HELPER UTILS. NORMALLY YOU DO NOT NEED TO USE THEM

;; Stage 1. Translation of complex ops into basis (atoms + conjunction, disjunction, negation)
(declare ^:private apply-translation-to-basis)

(def ^:private apply-translation-to-basis-table
  "No arguments are used."
  (list

   ;;  Expressions, which are `atoms` - constants, variables
   ;;  should be passed as is.
   [(fn [expr] (or (constant? expr)
                   (variable? expr)))
    (fn [expr _] expr)]

   [(fn [expr] (conjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (conjunction (apply-translation-to-basis a)
                                (apply-translation-to-basis b))))]

   [(fn [expr] (disjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (disjunction (apply-translation-to-basis a)
                                (apply-translation-to-basis b))))]

   [(fn [expr] (negation? expr))
    (fn [expr _] (let [[arg] (args expr)]
                   (negation (apply-translation-to-basis arg))))]

   [(fn [expr] (implication? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (disjunction (negation (apply-translation-to-basis a))
                                (apply-translation-to-basis b))))]))

(defn apply-translation-to-basis
  [expr]
  (apply-translation-table expr apply-translation-to-basis-table))


;; Stage 2 & 3. Push negation to atoms and remove all double-negations
(declare ^:private push-negation-to-atoms-with-carry)

(def ^:private push-negation-to-atoms-table
  "Uses single argument - `use-negation`, which stores current negation-carry state"
  (list

   ;;  Expressions, which are `atoms` - constants, variables
   ;;  should be marked with pushed negation, if needed.
   [(fn [expr] (or (constant? expr)
                   (variable? expr)))
    (fn [expr [use-negation]] (if use-negation
                                (negation expr)
                                expr))]

   [(fn [expr] (conjunction? expr))
    (fn [expr [use-negation]] (let [[a b] (args expr)
                                    op (if use-negation disjunction conjunction)] ;; Apply De Morgan rules, if needed
                                (op (push-negation-to-atoms-with-carry a use-negation)
                                    (push-negation-to-atoms-with-carry b use-negation))))]

   [(fn [expr] (disjunction? expr))
    (fn [expr [use-negation]] (let [[a b] (args expr)
                                    op (if use-negation conjunction disjunction)] ;; Apply De Morgan rules, if needed
                                (op (push-negation-to-atoms-with-carry a use-negation)
                                    (push-negation-to-atoms-with-carry b use-negation))))]

   [(fn [expr] (negation? expr))
    (fn [expr [use-negation]] (let [[arg] (args expr)]
                                (push-negation-to-atoms-with-carry arg (not use-negation))))]))

(defn- push-negation-to-atoms-with-carry
  [expr use-negation]
  (apply-translation-table expr push-negation-to-atoms-table use-negation))

(defn push-negation-to-atoms
  [expr]
  (push-negation-to-atoms-with-carry expr false))

;; Stage 4. Apply distribution rules
(declare apply-distribution-rules)

(def ^:private apply-distribution-rules-table
  "No arguments are used."
  (list

   ;;  Expressions, which are `atoms` - constants, variables or their's negation
   ;;  should be passed as is.
   [(fn [expr] (or (constant? expr)
                   (variable? expr)

                   (and (negation? expr)
                        (if-let [[arg] (args expr)]
                          (or (constant? arg)
                              (variable? arg))
                          false))))
    (fn [expr _] expr)]

   [(fn [expr] (disjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (disjunction (apply-distribution-rules a)
                                (apply-distribution-rules b))))]

   ;; Left distribution: (x v y) ^ z => (x ^ z) v (y ^ z)
   ;; Here x, y and z can be nested expressions as well
   [(fn [expr] (and (conjunction? expr)
                    (disjunction? (first (args expr)))))
    (fn [expr _] (let [[x-disjunction-y z] (args expr)
                       [x y] (args x-disjunction-y)]
                   (disjunction (conjunction (apply-distribution-rules x)
                                             (apply-distribution-rules z))
                                (conjunction (apply-distribution-rules y)
                                             (apply-distribution-rules z)))))]

   ;; Right distribution: x ^ (y v z) => (x ^ y) v (x ^ z)
   ;; Here x, y and z can be nested expressions as well
   [(fn [expr] (and (conjunction? expr)
                    (disjunction? (second (args expr)))))
    (fn [expr _] (let [[x y-disjunction-z] (args expr)
                       [y z] (args y-disjunction-z)]
                   (disjunction (conjunction (apply-distribution-rules x)
                                             (apply-distribution-rules y))
                                (conjunction (apply-distribution-rules x)
                                             (apply-distribution-rules z)))))]
   

   [(fn [expr] (conjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (conjunction (apply-distribution-rules a)
                                (apply-distribution-rules b))))]
   
   ))

(defn apply-distribution-rules
  [expr]
  (apply-translation-table expr apply-distribution-rules-table))



;; PUBLIC FUNCTIONS WHICH YOU NORMALLY HAVE TO USE

(defn make-dnf
  [expr]
  (->>
   expr
   (apply-translation-to-basis) ;; Stage 1
   (push-negation-to-atoms) ;; Stage 2 & 3
   (apply-distribution-rules))) ;; Stage 4