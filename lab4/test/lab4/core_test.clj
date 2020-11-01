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

;; EXPRESSION IN BASIS

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

;; EXPRESSION OF COMPLEX

(test/deftest test-expression-of-complex
  (let [var1 (alcore/variable :x)
        var2 (alcore/variable :y)
        const-true (alcore/constant true)
        const-false (alcore/constant false)]

    (test/testing "Testing expression of implication"
      (let [impl1 (alcore/implication var1 var2)
            impl2 (alcore/implication var1 const-true)

            expr1 (alcore/express-in-basis impl1)
            expr2 (alcore/express-in-basis impl2)]
        (test/is (alcore/disjunction? expr1))
        (test/is (alcore/disjunction? expr2))))))

(test/run-tests 'lab4.core-test)