(ns rimodo.model.model-type-test
  (:require [clojure.test :refer :all]
            [rimodo.model.model-type :refer :all]
            [rimodo.model.core :as c]))

(model-element-type :elemtyp1)
(model-element-type :elemtyp2)

(defn fixture [f]
  (elemtyp1 foo1)
  (elemtyp1 foo2)
  (elemtyp2 bar1)
  (f)
  (c/reset-registers!)
  (reset-mt!))

(use-fixtures :once fixture)

(defmacro count-tester
  ([result-count seq-creator msg]
  `(is (= ~result-count (count ~seq-creator)) ~msg))
  ([result-count seq-creator]
  `(is (= ~result-count (count ~seq-creator)) "")))

(deftest behaviour-injector
  (testing "model type injector"
    (count-tester 0 (injected-behaviours :elemtyp1))
    (inject-behaviour :elemtyp1 (fn[& args] nil))
    (count-tester 0 (injected-behaviours :elemtyp2) "this should be still zero as not global")
    (count-tester 1 (injected-behaviours :elemtyp1) "behaviour attached to here")))

(deftest model-type-unique-name
  (testing "name should be unique within model-type"
    (is (elemtyp1 foo3) "this should work fine")       
;    (is (= 0 (count (c/validation-errors :unique-name))) "there should be none")
    (elemtyp1 foo2))
;    (is (= 1 (count (c/validation-errors :unique-name))) "there should be one"))
  (testing "unique name is defined under model-type namespace"
    (is (ns-resolve (in-ns 'model.elemtyp1) 'foo1) "this should exists under model.elemtyp1 namespace")))

