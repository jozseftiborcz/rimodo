(ns rimodo.model.core-test
  (:require [clojure.test :refer :all]
            [rimodo.model.core :refer :all]
            [rimodo.model.model-type :as mt]))

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
  (reset-registers!))

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

(deftest model-lifecycle-testing
  (testing "creating model"
    (let [orig (count (all-models))]
      (is (= ((create-model "model1") :model-name)) "creates and returns the new model")
      (is (= (inc orig) (count (all-models))) "there should be one more")
      (is (= ((create-model "model1") :model-name)) "returns the existing model")
      (is (= (inc orig) (count (all-models))) "there should not one more")))
  (testing "finding model"
    (is (not (find-model "not-existing-model")) "this should return nil")
    (is (= ((find-model "model1") :model-name "model1")) "this should return the model")
    (is (= ((find-model (find-model "model1")) :model-name "model1")) "this should return the model too"))
  (testing "removing model"
    (is (not (remove-model "not-existing")) "removing a non-existing one is ok")
    (is (remove-model "model1") "removing an existing one returns that")
    (is (not (find-model "model1")) "this should return nil"))
  (testing "with-model"
    (set-model (create-model "model3"))
    (with-model (create-model "model2")
      (is (= (*model* :model-name) "model2")))
    (is (= (*model* :model-name) "model3"))))

