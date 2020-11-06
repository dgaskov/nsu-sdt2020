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

(defn atom?
  [expr]
  ((some-fn constant?
            variable?)
   expr))

(defn operation? [expr]
  ((some-fn conjunction?
            disjunction?
            negation?)
   expr))

(defn operation [expr]
  (when (operation? expr)
    (cond
      (conjunction? expr) conjunction
      (disjunction? expr) disjunction
      (negation? expr) negation)))

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

(def ^:private apply-translation-to-basis--table
  "No arguments are used."
  (list

   ;;  Expressions, which are `atoms` - constants, variables
   ;;  should be passed as is.
   [(fn [expr] (atom? expr))
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
  (apply-translation-table expr apply-translation-to-basis--table))


;; Stage 2 & 3. Push negation to atoms and remove all double-negations
(declare ^:private push-negation-to-atoms-with-carry)

(def ^:private push-negation-to-atoms--table
  "Uses single argument - `use-negation`, which stores current negation-carry state"
  (list

   ;;  Expressions, which are `atoms` - constants, variables
   ;;  should be marked with pushed negation, if needed.
   [(fn [expr] (atom? expr))
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
  (apply-translation-table expr push-negation-to-atoms--table use-negation))

(defn push-negation-to-atoms
  [expr]
  (push-negation-to-atoms-with-carry expr false))

;; Stage 4. Apply distribution rules
(declare apply-distribution-rules)

(def ^:private apply-distribution-rules--table
  "No arguments are used."
  (list

   ;;  Expressions, which are `atoms` - constants, variables or their's negation
   ;;  should be passed as is.
   [(fn [expr] (or (atom? expr)

                   (and (negation? expr)
                        (if-let [[arg] (args expr)]
                          (atom? arg)
                          false))))
    (fn [expr _] expr)]

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

   [(fn [expr] (disjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (disjunction (apply-distribution-rules a)
                                (apply-distribution-rules b))))]

   [(fn [expr] (conjunction? expr))
    (fn [expr _] (let [[a b] (args expr)]
                   (conjunction (apply-distribution-rules a)
                                (apply-distribution-rules b))))]))

(defn apply-distribution-rules
  [expr]
  (apply-translation-table expr apply-distribution-rules--table))


;; SIGNIFYING VARIABLES: i. e. replace `x` with `false`
(declare signify-variable-with-value)

(def ^:private signify-variable-with-value--table
  (list
   [(fn [expr] (constant? expr))
    (fn [expr _] expr)]

   [(fn [expr] (variable? expr))
    (fn [expr [var val]] (if (same-variables? expr var)
                           (constant val)
                           expr))]

   [(fn [expr] (conjunction? expr))
    (fn [expr [var val]] (let [[a b] (args expr)]
                           (conjunction (signify-variable-with-value a var val)
                                        (signify-variable-with-value b var val))))]

   [(fn [expr] (disjunction? expr))
    (fn [expr [var val]] (let [[a b] (args expr)]
                           (disjunction (signify-variable-with-value a var val)
                                        (signify-variable-with-value b var val))))]

   [(fn [expr] (negation? expr))
    (fn [expr [var val]] (let [[arg] (args expr)]
                           (negation (signify-variable-with-value arg var val))))]))

(defn- signify-variable-with-value
  [expr var val]
  (apply-translation-table expr signify-variable-with-value--table var val))


;; SIMPLIFYING EXPRESSION
(declare simplify)

(def simplify--table
  (list

   ;; Idempotation 1: a ^ a == a
   [(fn [expr] (and (conjunction? expr)
                    (let [[a b] (args expr)]
                      (= a b))))
    (fn [expr _] (simplify (first (args expr))))]

   ;; Idempotation 2: a v a == a
   [(fn [expr] (and (disjunction? expr)
                    (let [[a b] (args expr)]
                      (= a b))))
    (fn [expr _] (simplify (first (args expr))))]

   ;; Idempotation 3: a ^ false == false
   [(fn [expr] (and (conjunction? expr)
                    (let [[a b] (args expr)
                          const-false (constant false)]
                      (or (= a const-false)
                          (= b const-false)))))
    (fn [_ _] (constant false))]

   ;; Idempotation 4: a ^ true == a
   [(fn [expr] (and (conjunction? expr)
                    (let [[a b] (args expr)
                          const-true (constant true)]
                      (or (= a const-true)
                          (= b const-true)))))
    (fn [expr _] (let [[a b] (args expr)
                       const-true (constant true)]
                   (simplify (if (= a const-true) b a))))]

   ;; Idempotation 5: a v false == a
   [(fn [expr] (and (disjunction? expr)
                    (let [[a b] (args expr)
                          const-false (constant false)]
                      (or (= a const-false)
                          (= b const-false)))))
    (fn [expr _] (let [[a b] (args expr)
                       const-false (constant false)]
                   (simplify (if (= a const-false) b a))))]

   ;; Idempotation 6: a v true == true
   [(fn [expr] (and (disjunction? expr)
                    (let [[a b] (args expr)
                          const-true (constant true)]
                      (or (= a const-true)
                          (= b const-true)))))
    (fn [_ _] (constant true))]

   ;; Contradiction law: a ^ !a == false
   [(fn [expr] (and (conjunction? expr)
                    (let [[a b] (args expr)]
                      (letfn [(equality-with-negation [expr1 expr2]
                                (and (negation? expr1)
                                     (let [[arg] (args expr1)]
                                       (= arg expr2))))]
                        (or (equality-with-negation a b)
                            (equality-with-negation b a))))))
    (fn [_ _] (constant false))]

   ;; Excluded third law: a v !a == true
   [(fn [expr] (and (disjunction? expr)
                    (let [[a b] (args expr)]
                      (letfn [(equality-with-negation [expr1 expr2]
                                (and (negation? expr1)
                                     (let [[arg] (args expr1)]
                                       (= arg expr2))))]
                        (or (equality-with-negation a b)
                            (equality-with-negation b a))))))
    (fn [_ _] (constant true))]

   ;; Otherwise, leave expression as is, if it's atom
   [(fn [expr] (atom? expr))
    (fn [expr _] expr)]

   ;; Or apply the same operation, if it is
   [(fn [expr] (operation? expr))
    (fn [expr _] (let [op (operation expr)
                       args (args expr)]
                   (apply op (map simplify args))))]))

;; PUBLIC FUNCTIONS WHICH YOU NORMALLY HAVE TO USE

(defn make-dnf
  [expr]
  (->>
   expr
   (apply-translation-to-basis) ;; Stage 1
   (push-negation-to-atoms) ;; Stage 2 & 3
   (apply-distribution-rules) ;; Stage 4
   (simplify))) ;; Stage 4+

(defn signify-variable
  [expr var val]
  {:pre [(variable? var)
         (boolean? val)]}
  (signify-variable-with-value expr var val))

(defn signify-variable-and-make-dnf
  [expr var val]
  (make-dnf (signify-variable-with-value expr var val)))

(defn simplify
  [expr]
  (apply-translation-table expr simplify--table))