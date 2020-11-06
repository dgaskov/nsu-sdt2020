(ns lab4.core-test
  (:require [clojure.test :as test]
            [lab4.bool-algebra-core :as alcore]))



;; CONSTANTS

(test/deftest test-constants
  (test/testing "Testing constants"
    (test/is (thrown? java.lang.AssertionError (alcore/constant 1)))
    (test/is (alcore/constant? (alcore/constant false)))
    (test/is (= true (alcore/constant-value (alcore/constant true))))))



;; VARIABLES

(test/deftest test-variables
  (test/testing "Testing variables"
    (test/is (alcore/variable? (alcore/variable :x)))
    (test/is (= :x (alcore/variable-name (alcore/variable :x))))
    (test/is (alcore/same-variables? (alcore/variable :x)
                                     (alcore/variable :x)))
    (test/is (not (alcore/same-variables? (alcore/variable :x)
                                          (alcore/variable :y))))))



;; OPERATORS

(test/deftest test-operators
  (let [arg1 (alcore/variable :x)
        arg2 (alcore/variable :y)
        arg3 (alcore/constant false)]

    (test/testing "Testing conjunction"
      (test/are [expr] (alcore/conjunction? expr)
        (alcore/conjunction arg1 arg2)
        (alcore/conjunction arg1 arg3)
        (alcore/conjunction arg2 arg3)
        (alcore/conjunction arg1 arg1)))

    (test/testing "Testing disjunction"
      (test/are [expr] (alcore/disjunction? expr)
        (alcore/disjunction arg1 arg2)
        (alcore/disjunction arg1 arg3)
        (alcore/disjunction arg2 arg3)
        (alcore/disjunction arg1 arg1)))

    (test/testing "Testing negation"
      (test/are [expr] (alcore/negation? expr)
        (alcore/negation arg1)
        (alcore/negation arg2)
        (alcore/negation arg3)))

    (test/testing "Testing implication"
      (test/are [expr] (alcore/implication? expr)
        (alcore/implication arg1 arg2)
        (alcore/implication arg1 arg3)
        (alcore/implication arg2 arg3)
        (alcore/implication arg1 arg1)))))



;; STAGE 1. EXPRESSION OF BASIS OPS - must be identity

(test/deftest test-expression-of-basis
  (let [var1 (alcore/variable :x)
        var2 (alcore/variable :y)
        const-true (alcore/constant true)
        const-false (alcore/constant false)]

    (test/testing "Testing expression of constants"
      (let [expr1 (alcore/apply-translation-to-basis const-true)
            expr2 (alcore/apply-translation-to-basis const-false)]
        (test/is (= expr1 const-true))
        (test/is (and (alcore/constant? expr2) ; More granular validation
                      (= false (alcore/constant-value expr2))))))

    (test/testing "Testing expression of variables"
      (let [expr1 (alcore/apply-translation-to-basis var1)
            expr2 (alcore/apply-translation-to-basis var2)]
        (test/is (= expr1 var1))
        (test/is (and (alcore/variable? expr2) ; More granular validation
                      (not= :x (alcore/constant-value expr2))))))

    (test/testing "Testing expression of conjunction"
      (let [conj1 (alcore/conjunction var1 var2)
            conj2 (alcore/conjunction var1 const-true)

            expr1 (alcore/apply-translation-to-basis conj1)
            expr2 (alcore/apply-translation-to-basis conj2)]
        (test/is (= expr1 conj1))
        (test/is (= expr2 conj2))))

    (test/testing "Testing expression of disjunction"
      (let [disj1 (alcore/disjunction var1 var2)
            disj2 (alcore/disjunction var1 const-true)

            expr1 (alcore/apply-translation-to-basis disj1)
            expr2 (alcore/apply-translation-to-basis disj2)]
        (test/is (= expr1 disj1))
        (test/is (= expr2 disj2))))

    (test/testing "Testing expression of negation"
      (let [neg1 (alcore/negation const-true)
            neg2 (alcore/negation var1)

            expr1 (alcore/apply-translation-to-basis neg1)
            expr2 (alcore/apply-translation-to-basis neg2)]
        (test/is (= expr1 neg1))
        (test/is (= expr2 neg2))))))



;; STAGE 1. EXPRESSION OF COMPLEX OPS
;; (implication, XOR i. e.) - must be converted to basis

(test/deftest test-expression-of-complex
  (let [var1 (alcore/variable :x)
        var2 (alcore/variable :y)
        const-true (alcore/constant true)
        const-false (alcore/constant false)]

    (test/testing "Testing expression of implication"
      (let [impl (alcore/implication var1 var2)
            expr (alcore/apply-translation-to-basis impl)

            [a b] (alcore/args expr)]
        (test/is (alcore/disjunction? expr))
        (test/is (= a (alcore/negation var1)))
        (test/is (= b var2))))

    (test/testing "Testing expression of nested implication"
      (let [;; (x -> y)
            impl (alcore/implication var1 var2)

            ;; (x -> y) ^ true
            conj2 (alcore/conjunction impl const-true)

            ;; Must be (!x v y) ^ true
            expr1 (alcore/apply-translation-to-basis conj2)]

        ;; Example of long, var-by-var testing
        (test/is (alcore/conjunction? expr1))
        (let [[disj ttrue] (alcore/args expr1)]
          (test/is (alcore/disjunction? disj))
          (test/is (alcore/constant? ttrue))
          (let [[not-var1 vvar2] (alcore/args disj)
                [vvar1] (alcore/args not-var1)]
            (test/is (alcore/negation? not-var1))
            (test/is (= var1 vvar1))
            (test/is (= var2 vvar2))))
        ;; Same result, using construction of end expression
        (test/is (= expr1
                    (alcore/conjunction (alcore/disjunction (alcore/negation var1) var2)
                                        (alcore/constant true))))))))



;; STAGE 2 & 3. PUSH NEGATION TO ATOMS

(test/deftest test-push-negation-to-atoms
  (let [x (alcore/variable :x)
        y (alcore/variable :y)]
    (test/testing "Testing simple negation push: !(x ^ y) ==> (!x v !y)"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/disjunction x y)))

                  (alcore/conjunction (alcore/negation x)
                                      (alcore/negation y)))))

    (test/testing "Testing simple negation push: !(x v !y) ==> (!x ^ y)"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/conjunction (alcore/negation x) y)))

                  (alcore/disjunction x (alcore/negation y)))))

    (test/testing "Testing negation push: (x ^ !(x v y)) ==> (x ^ (!x ^ !y))"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/conjunction x (alcore/negation (alcore/disjunction x y))))

                  (alcore/conjunction x (alcore/conjunction (alcore/negation x)
                                                            (alcore/negation y))))))

    (test/testing "Testing complex negation push: !(x ^ !(!y v x)) ==> (!x v (!y v x))"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/conjunction x (alcore/negation
                                                           (alcore/disjunction (alcore/negation y)
                                                                               x)))))

                  (alcore/disjunction (alcore/negation x)
                                      (alcore/disjunction (alcore/negation y)
                                                          x)))))

    (test/testing "Testing multply negation: !!x ==> x"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/negation x)))

                  x)))))



;; STAGE 4. APPLY DISTRIBUTION RULES

(test/deftest test-applying-distribution
  (let [x (alcore/variable :x)
        y (alcore/variable :y)
        z (alcore/variable :z)]

    (test/testing "Testing applying simple left distribution: (x v y) ^ z ==> (x ^ z) v (y ^ z)"
      (test/is (= (alcore/apply-distribution-rules
                   (alcore/conjunction (alcore/disjunction x y)
                                       z))

                  (alcore/disjunction (alcore/conjunction x z)
                                      (alcore/conjunction y z)))))

    (test/testing "Testing applying simple right distribution: x ^ (y v z) ==> (x ^ y) v (x ^ z)"
      (test/is (= (alcore/apply-distribution-rules
                   (alcore/conjunction x
                                       (alcore/disjunction y z)))

                  (alcore/disjunction (alcore/conjunction x y)
                                      (alcore/conjunction x z)))))))



;; SIMPLIFYING EXPRESSION

(test/deftest test-simplify
  (let [a (alcore/variable :a)
        b (alcore/variable :b)
        const-false (alcore/constant false)
        const-true (alcore/constant true)]

    (test/testing "Testing simplification of idempotation 1"
      (test/is (= (alcore/simplify
                   (alcore/conjunction a a))
                  a)))

    (test/testing "Testing simplification of idempotation 2"
      (test/is (= (alcore/simplify
                   (alcore/disjunction a a))
                  a)))

    (test/testing "Testing simplification of idempotation 3"
      (test/is (= (alcore/simplify
                   (alcore/conjunction a const-false))
                  const-false)))

    (test/testing "Testing simplification of idempotation 4"
      (test/is (= (alcore/simplify
                   (alcore/conjunction a const-true))
                  a)))

    (test/testing "Testing simplification of idempotation 5"
      (test/is (= (alcore/simplify
                   (alcore/disjunction a const-false))
                  a)))

    (test/testing "Testing simplification of idempotation 6"
      (test/is (= (alcore/simplify
                   (alcore/disjunction a const-true))
                  const-true)))

    (test/testing "Testing simplification of contradiction law"
      (test/is (= (alcore/simplify
                   (alcore/conjunction a (alcore/negation a)))
                  const-false))

      (test/is (= (alcore/simplify
                   (alcore/conjunction (alcore/negation a) a))
                  const-false)))

    (test/testing "Testing simplification of excluded third law"
      (test/is (= (alcore/simplify
                   (alcore/disjunction a (alcore/negation a)))
                  const-true))

      (test/is (= (alcore/simplify
                   (alcore/disjunction (alcore/negation a) a))
                  const-true)))

    (test/testing "Testing simplification of constant"
      (test/is (= (alcore/simplify
                   const-true)
                  const-true)))

    (test/testing "Testing simplification of variable"
      (test/is (= (alcore/simplify
                   a)
                  a)))

    (test/testing "Testing simplification of conjunction"
      (test/is (= (alcore/simplify
                   (alcore/conjunction a b))
                  (alcore/conjunction a b))))

    (test/testing "Testing simplification of negation"
      (test/is (= (alcore/simplify
                   (alcore/negation a))
                  (alcore/negation a))))))


;; DNF. COMBINATION OF ALL THE METHODS ABOVE

(test/deftest test-make-dnf
  (let [a (alcore/variable :a)
        b (alcore/variable :b)
        c (alcore/variable :c)
        d (alcore/variable :d)]

    (test/testing "Testing creation of DNF: ((a & !b) | c) -> (d | !(b -> c)) ==> ((!a & !c) | (b & !c)) | (d | (b & !c))"
      (test/is (= (alcore/make-dnf
                   (alcore/implication (alcore/disjunction (alcore/conjunction a (alcore/negation b))
                                                           c)
                                       (alcore/disjunction d
                                                           (alcore/negation (alcore/implication b c)))))

                  (alcore/disjunction (alcore/disjunction (alcore/conjunction (alcore/negation a)
                                                                              (alcore/negation c))
                                                          (alcore/conjunction b
                                                                              (alcore/negation c)))
                                      (alcore/disjunction d
                                                          (alcore/conjunction b
                                                                              (alcore/negation c)))))))))



;; SIGNIFYING VARIABLES

(test/deftest test-signifying-variables
  (let [x (alcore/variable :x)
        y (alcore/variable :y)

        expr1 (alcore/conjunction x y)
        expr2 (alcore/disjunction (alcore/conjunction x y)
                                  (alcore/negation (alcore/conjunction (alcore/negation x)
                                                                       y)))]
    (test/testing "Testing simple variable signifying: x ==> true"
      (test/is (= (alcore/signify-variable x x true)
                  (alcore/constant true))))

    (test/testing "Testing simple variable signifying: (x ^ y) ==> (true ^ y)"
      (test/is (= (alcore/signify-variable expr1 x true)
                  (alcore/conjunction (alcore/constant true)
                                      y))))

    (test/testing "Testing complex variable signifying: (x ^ y) | !(!x ^ y) ==> (false ^ y) | !(!false ^ y)"
      (test/is (= (alcore/signify-variable expr2 x false)
                  (alcore/disjunction (alcore/conjunction (alcore/constant false)
                                                          y)
                                      (alcore/negation (alcore/conjunction (alcore/negation (alcore/constant false))
                                                                           y))))))))

(test/run-tests 'lab4.core-test)