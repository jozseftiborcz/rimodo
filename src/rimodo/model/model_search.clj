(ns rimodo.model.model-search
  (:require [rimodo.model.core :as core]))

(defn re-pattern?
  [s]
  (= (type s) java.util.regex.Pattern))

(defn search
  [ & conds]
  (let [extract-type-fn (fn[x] ((meta ((meta x) :model-type)) :name))
        match-type-fn (fn[x] (= (extract-type-fn x) 
                                (symbol (name (first conds)))))]
    (when conds
      (if (core/model-type? (first conds))
        (if-let [me (second conds)]
          (filter #(and (match-type-fn %)
                        (if (re-pattern? me) 
                          (re-find me (name ((meta %) :name)))
                          (= ((meta %) :name )
                             (symbol me)))) 
                  (core/model-elements))
          (filter #(match-type-fn %)
                  (core/model-elements)))))))


