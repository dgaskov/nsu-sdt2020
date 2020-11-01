(ns lab4.core-test
  (:require [clojure.test :as test]
            [lab4.bool-algebra-core :as core]))

(test/deftest test-constants
  (test/testing "Testing constants"
    (test/is (thrown? java.lang.AssertionError (core/constant 1)))
    (test/is (core/constant? (core/constant false)))
    (test/is (= true (core/constant-value (core/constant true))))))

(test/deftest test-variables
  (test/testing "Testing variables"
    (test/is (core/variable? (core/variable :x)))
    (test/is (= :x (core/variable-name (core/variable :x))))
    (test/is (core/same-variables? (core/variable :x)
                                   (core/variable :x)))
    (test/is (not (core/same-variables? (core/variable :x)
                                        (core/variable :y))))))

(test/deftest test-operators-
  (let [arg1 (core/variable :x)
        arg2 (core/variable :y)
        arg3 (core/constant false)]
    (test/testing "Testing conjunction"
      (test/are [expr] (core/conjunction? expr)
        (core/conjunction arg1 arg2)
        (core/conjunction arg1 arg3)
        (core/conjunction arg2 arg3)
        (core/conjunction arg1 arg1)))

    (test/testing "Testing disjunction"
      (test/are [expr] (core/disjunction? expr)
        (core/disjunction arg1 arg2)
        (core/disjunction arg1 arg3)
        (core/disjunction arg2 arg3)
        (core/disjunction arg1 arg1)))

    (test/testing "Testing negation"
      (test/are [expr] (core/negation? expr)
        (core/negation arg1)
        (core/negation arg2)
        (core/negation arg3)))

    (test/testing "Testing implication"
      (test/are [expr] (core/implication? expr)
        (core/implication arg1 arg2)
        (core/implication arg1 arg3)
        (core/implication arg2 arg3)
        (core/implication arg1 arg1)))))

(test/run-tests 'lab4.core-test)