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

(deftest type-search
  (search-tester 2 :elemtyp1)
  (search-tester 1 :elemtyp2))

(deftest name-search
  (search-tester 2 #".*1")
  (search-tester 1 "foo2"))

(deftest key-value-search
  (search-tester 2 {:foo 1})
  (search-tester 1 {:foo 1 :bar 2}))

(deftest mixed-searches
  (search-tester 1 :elemtyp1 "foo1")
  (search-tester 0 :elemtyp1 "foo3-not-defined")
  (search-tester 1 :elemtyp2 bar1)
  (search-tester 2 :elemtyp1 #"foo?")
  (search-tester 1 :elemtyp1 #".*2"))

(deftest set-option
  (search-tester 3 #{:elemtyp1 :elemtyp2})
  (search-tester 0 #{:elemtyp1} #{:elemtyp2})
  (search-tester 3 #{:elemtyp1 :elemtyp2} #{:elemtyp1 :elemtyp2})
  (search-tester 3 #{:elemtyp1 {:bar 2}}))
