(ns rimodo.model.core-test
  (:require [clojure.test :refer :all]
            [rimodo.model.core :refer :all]
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

(deftest model-element?-testing
  (testing "arbitrary input returns false"
    (is (not (model-element? (fn[& args] ))) "this is not an me")
    (is (not (model-element? "foo")) "this isn't too")
    (is (not (model-element? :foo)) "by keyword it cannot find"))
  (testing "valid results"
    (is (model-element? foo2)) "this is it"))

(deftest model-type?-testing
  (is (not (model-type? "not-model-type")) "this is not")
  (is (not (model-type? foo2)) "this is not")
  (is (model-type? :elemtyp2) "works with keyword")
  (is (model-type? #'elemtyp2) "works with var")
  (is (model-type? "elemtyp1") "works with string too"))


