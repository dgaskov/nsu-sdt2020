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
      (let [expr1 (alcore/express-in-basis const-true)
            expr2 (alcore/express-in-basis const-false)]
        (test/is (= expr1 const-true))
        (test/is (and (alcore/constant? expr2) ; More granular validation
                      (= false (alcore/constant-value expr2))))))

    (test/testing "Testing expression of variables"
      (let [expr1 (alcore/express-in-basis var1)
            expr2 (alcore/express-in-basis var2)]
        (test/is (= expr1 var1))
        (test/is (and (alcore/variable? expr2) ; More granular validation
                      (not= :x (alcore/constant-value expr2))))))

    (test/testing "Testing expression of conjunction"
      (let [conj1 (alcore/conjunction var1 var2)
            conj2 (alcore/conjunction var1 const-true)

            expr1 (alcore/express-in-basis conj1)
            expr2 (alcore/express-in-basis conj2)]
        (test/is (= expr1 conj1))
        (test/is (= expr2 conj2))))

    (test/testing "Testing expression of disjunction"
      (let [disj1 (alcore/disjunction var1 var2)
            disj2 (alcore/disjunction var1 const-true)

            expr1 (alcore/express-in-basis disj1)
            expr2 (alcore/express-in-basis disj2)]
        (test/is (= expr1 disj1))
        (test/is (= expr2 disj2))))

    (test/testing "Testing expression of negation"
      (let [neg1 (alcore/negation const-true)
            neg2 (alcore/negation var1)

            expr1 (alcore/express-in-basis neg1)
            expr2 (alcore/express-in-basis neg2)]
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
            expr (alcore/express-in-basis impl)

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
            expr1 (alcore/express-in-basis conj2)]

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

;; STAGE 2. PUSH NEGATION TO ATOMS

(test/deftest test-push-negation-to-atoms
  (let [x (alcore/variable :x)
        y (alcore/variable :y)]
    (test/testing "Testing simple negation push: !(x ^ y) => (!x v !y)"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/disjunction x y)))

                  (alcore/conjunction (alcore/negation x)
                                      (alcore/negation y)))))

    (test/testing "Testing simple negation push: !(x v !y) => (!x ^ y)"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/conjunction (alcore/negation x) y)))

                  (alcore/disjunction x (alcore/negation y)))))

    (test/testing "Testing negation push: (x ^ !(x v y)) => (x ^ (!x ^ !y))"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/conjunction x (alcore/negation (alcore/disjunction x y))))

                  (alcore/conjunction x (alcore/conjunction (alcore/negation x)
                                                            (alcore/negation y))))))

    (test/testing "Testing complex negation push: !(x ^ !(!y v x)) => (!x v (!y v x))"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/conjunction x (alcore/negation
                                                           (alcore/disjunction (alcore/negation y)
                                                                               x)))))

                  (alcore/disjunction (alcore/negation x)
                                      (alcore/disjunction (alcore/negation y)
                                                          x)))))
    
    (test/testing "Testing multply negation: !!x => x"
      (test/is (= (alcore/push-negation-to-atoms
                   (alcore/negation (alcore/negation x)))
                  
                  x)))))

(test/run-tests 'lab4.core-test)