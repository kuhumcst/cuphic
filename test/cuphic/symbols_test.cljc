(ns cuphic.symbols-test
  (:require [clojure.test :refer [deftest is are testing]]
            [cuphic.symbols :as syms]))

(deftest test-slot-type
  (testing "expected slot types for different pattern nodes"
    (are [pnode k] (= (syms/slot-type pnode) k)
      'variable :variable
      'variable? :variable
      'variable??? :variable
      '...variable :variable
      '?optional-variable :optional-variable
      '???optional-variable :optional-variable
      '(??? pnode k z) :optional-repetition
      '(... pnode k z) :definite-repetition
      '... :definite-omission
      '??? :optional-omission
      '_ :wildcard
      '? :optional)))
