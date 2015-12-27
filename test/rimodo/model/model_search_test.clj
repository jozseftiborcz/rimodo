(ns rimodo.model.model-search-test
  (:require [clojure.test :refer :all]
            [rimodo.model.model-search :refer :all]
            [rimodo.model.model-type :as mt]))

(mt/model-element-type :elemtyp1)
(mt/model-element-type :elemtyp2)

(defn fixture [f]
  (elemtyp1 foo1)
  (elemtyp1 foo2)
  (elemtyp2 bar1)
  (f))

(use-fixtures :once fixture)

(deftest model-element-type-search
  (testing "first parameter is a keyword and a valid model element type"
    (is (= 2 (count (search :elemtyp1))))
    (is (= 1 (count (search :elemtyp2)))))
  (testing "first parameter is a string and a valid model element type"
    (is (= 2 (count (search "elemtyp1")))))
  (testing "second parameter is a model-element as name"
    (is (= 1 (count (search :elemtyp1 "foo1"))))
    (is (= 0 (count (search :elemtyp1 "foo3-not-defined")))))
  (testing "second parameter is a model-element as var"
    (is (= 1 (count (search :elemtyp2 bar1)))))
  (testing "second parameter is a model-element as regex"
    (is (= 2 (count (search :elemtyp1 #"foo?"))))
    (is (= 1 (count (search :elemtyp1 #".*2"))))))


