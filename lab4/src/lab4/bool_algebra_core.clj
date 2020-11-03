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

(defn- boolean-xor
  [a b]
  {:pre [(boolean? a)
         (boolean? b)]}
  (if a (if b false true) (if b true false)))

(defn args [expr]
  (rest expr))

(defn apply-translation-table
  "Appliex given transformation table to the expression.
   Transformation table is a list of pairs (in fact, nested lists).
   Nested list must contain a `rule` - pair of two values.
   First is a `predicate`, which defines should we apply
   this rule to the expression, or not (must return bool)
   Second is a `transform` - function which takes an expression
   with optional arguments, and returns new modified expression"
  [expr translation-table & args]
  (if-let [[transform, rule-index] (some (fn [[idx rule]]
                             (if ((first rule) expr)
                               [(second rule) idx]
                               false))
                           (map-indexed list translation-table))]
    (do
      (println "Apply rule with index" rule-index "and args" args)
      (transform expr args))
    (throw (ex-info
            "Could not found any rules for given expression"
            {:expresion expr
             :number-of-rules (count translation-table)}))))


;; BOOLEAN ENGINE HELPER UTILS. NORMALLY YOU DO NOT NEED TO USE THEM

;; Stage 1. Expression of complex ops

(declare ^:private express-in-basis)

(def ^:private express-in-basis-table
  "No arguments are used."
  (list
  ;;  Expressions, which are `atoms` - constants, variables
  ;;  should be passed as is.
   [(fn [expr] (or (constant? expr)
                   (variable? expr)))
    (fn [expr _] expr)]

   [(fn [expr] (conjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (conjunction (express-in-basis a)
                                (express-in-basis b))))]

   [(fn [expr] (disjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (disjunction (express-in-basis a)
                                (express-in-basis b))))]

   [(fn [expr] (negation? expr))
    (fn [expr _] (let [[arg] (args expr)]
                   (negation (express-in-basis arg))))]

   [(fn [expr] (implication? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (disjunction (negation a) b)))]))

(defn express-in-basis
  [expr]
  (apply-translation-table expr express-in-basis-table))


;; Stage 2. Push negation to atoms
(declare push-negation-to-atoms-with-arg)

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
                              (op (push-negation-to-atoms-with-arg a use-negation)
                                  (push-negation-to-atoms-with-arg b use-negation))))]

   [(fn [expr] (disjunction? expr))
    (fn [expr [use-negation]] (let [[a b] (args expr)
                                  op (if use-negation conjunction disjunction)] ;; Apply De Morgan rules, if needed
                              (op (push-negation-to-atoms-with-arg a use-negation)
                                  (push-negation-to-atoms-with-arg b use-negation))))]

   [(fn [expr] (negation? expr))
    (fn [expr [use-negation]] (let [[arg] (args expr)]
                              (push-negation-to-atoms-with-arg arg (boolean-xor use-negation true))))]))

(defn- push-negation-to-atoms-with-arg
  [expr use-negation]
  (apply-translation-table expr push-negation-to-atoms-table use-negation))

(defn push-negation-to-atoms
  [expr]
  (push-negation-to-atoms-with-arg expr false))