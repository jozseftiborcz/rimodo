(ns rimodo.model.model-search-test
  (:require [clojure.test :refer :all]
            [rimodo.model.model-search :refer :all]
            [rimodo.model.model-type :as mt]
            [rimodo.model.core :as c]))

(mt/model-element-type :elemtyp1)
(mt/model-element-type :elemtyp2)

(defn fixture [f]
  (elemtyp1 foo1)
  (foo1 :foo 1)
  (foo1 :bar 1)
  (elemtyp1 foo2)
  (elemtyp2 bar1)
  (bar1 :foo 1)
  (bar1 :bar 2)
  (f)
  (c/reset-registers!))

(use-fixtures :once fixture)

(defmacro search-tester
  [ result-count & conditions]
  `(is (= ~result-count (count (search ~@conditions)))))

(defmacro iv-search-tester
  [ result-count & conditions]
  `(is (= ~result-count (count (iv-search ~@conditions)))))

(deftest model-element-search
  (testing "type-search"
    (search-tester 2 :elemtyp1)
    (search-tester 1 :elemtyp2))

  (testing "name-search"
    (search-tester 2 #".*1")
    (search-tester 1 "foo2"))

  (testing "key-value-search"
    (search-tester 2 {:foo 1})
    (search-tester 1 {:foo 1 :bar 2}))

  (testing "mixed-searches"
    (search-tester 1 :elemtyp1 "foo1")
    (search-tester 0 :elemtyp1 "foo3-not-defined")
    (search-tester 1 :elemtyp2 bar1)
    (search-tester 2 :elemtyp1 #"foo?")
    (search-tester 1 :elemtyp1 #".*2"))

  (testing "set-option"
    (search-tester 3 #{:elemtyp1 :elemtyp2})
    (search-tester 0 #{:elemtyp1} #{:elemtyp2})
    (search-tester 3 #{:elemtyp1 :elemtyp2} #{:elemtyp1 :elemtyp2})
    (search-tester 3 #{:elemtyp1 {:bar 2}})))

(deftest invariant-violation-search
  (c/set-violations! :violation1 bar1 "something")
  (c/set-violations! :violation2 foo2 "something")
  (c/set-violations! :violation1 foo1 "something")

  (testing "by keyword"
    (iv-search-tester 2 :violation1))

  (testing "by model element"
    (iv-search-tester 1 foo1))

  (testing "by regexp"
    (iv-search-tester 2 #"1")))
