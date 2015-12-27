(ns rimodo.model.server
  (:require [rimodo.model.core :as core]
            [rimodo.model.model-type :as mt]))

(mt/model-element-type :server)
(mt/model-element-type :application)

(mt/with-meta-defn meph-cluster-member-handler
  {:model-element-param-handler true
   :doc "returns string representation of model element"
   :examples ["(me) returns its string rep."]}
  [element-sym model-state args]
;  (println "meph-cluster-member-handler" element-sym)
  (let [[kv-args rest-args] (mt/separate-kv-attrs args)
        servers (filter server? rest-args)]
;    (if (first rest-args) (println "x0" (type (first rest-args))))
;    (println "x1" (type element-sym) servers rest-args)
    (apply element-sym :cluster-members servers)
    (doseq [server servers] 
      ((core/find-me server) (keyword "cluster-member") element-sym))))

(mt/model-element-type :cluster-group meph-cluster-member-handler)

(defn runs-on [& args] (cons :runs-on args))

