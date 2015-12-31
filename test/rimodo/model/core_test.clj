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
    (is (not (model-element? (fn[& args] ))))
    (is (not (model-element? "foo")))
    (is (not (model-element? :foo)))
    (is (model-element? foo2))))

