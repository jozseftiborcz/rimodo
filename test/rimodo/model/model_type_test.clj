(ns rimodo.model.model-type-test
  (:require [clojure.test :refer :all]
            [rimodo.model.model-type :refer :all]
            [rimodo.model.core :as c]
            [rimodo.model.model-search :as ms]))

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
    (count-tester 1 (injected-behaviours :elemtyp1) "behaviour attached to here")
    (def x (atom 0))
    (inject-behaviour :elemtyp2 (fn[& args] (swap! x inc)))
    (elemtyp2 foo2)
    (is (= 1 @x) "behaviour is called"))

  (testing "global model type injector"
    (let [x (atom 0)]
      (inject-behaviour :global (fn[& args] (swap! x inc)))
      (elemtyp1 foo3)
      (elemtyp2 foo4)
      (is (= 2 @x) "behaviour is called twice"))))
  
(deftest model-type-unique-name
  (testing "name should be unique within model-type"
    (is (elemtyp1 foo5) "this should work fine")       
    (is (= 0 (count (ms/iv-search :unique-model-element-name))) "there should be none")
    (elemtyp1 foo2)
    (is (= 1 (count (ms/iv-search :unique-model-element-name))) "there should be one"))
  (testing "unique name is defined under model-type namespace"
    (is (ns-resolve (in-ns 'model.elemtyp1) 'foo1) "this should exists under model.elemtyp1 namespace")))

