(ns development-utils
  (:require [clojure.test :as t]))

(defn run-tests
  "Run tests"
  []
  (t/run-tests (find-ns 'rimodo.core-test)))
