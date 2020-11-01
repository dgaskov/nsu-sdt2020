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



;; UTILS

(defn args [expr]
  (rest expr))



;; ENGINE

(declare make-dnf)

(def expression-table
  (list
  ;;  Expressions, which are `atoms` - constants, varables,
  ;;  conjunction, disjunction and negation should be passed as is.
   [(fn [expr] (or (constant? expr)
                   (variable? expr)
                   (conjunction? expr)
                   (disjunction? expr)
                   (negation? expr)))
    identity]

   [(fn [expr] (implication? expr))
    (fn [expr] (let [[a b] (args expr)]
                 (disjunction (negation a) b)))]))

(defn express-in-basis
  [expr]
  (if-let [transform (some (fn [rule]
                             (if ((first rule) expr)
                               (second rule)
                               false))
                           expression-table)]
    (transform expr)
    (throw (ex-info
            "Could not found any rules for given expression"
            {:expresion expr
             :number-of-rules (count expression-table)}))))