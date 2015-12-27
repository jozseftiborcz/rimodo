(ns rimodo.model.model-search
  (:require [rimodo.model.core :as core]))

(defn- re-pattern?
  [s]
  (= (type s) java.util.regex.Pattern))

(defn search
  "It searches model elements by type"
  [ & conds]
  ;(let [extract-type-fn (fn[x] ((meta ((meta x) :model-type)) :name))
  (let [extract-type-fn (fn[x] (x :model-type))
        match-type-fn (fn[x] (= (symbol (name (extract-type-fn x))) 
                                (symbol (name (first conds)))))]
    (when conds
      (if (core/model-type? (first conds))
      ;(if false
        (if-let [me (second conds)]
          (filter #(and (match-type-fn %)
                        (cond 
                          (re-pattern? me) (re-find me (% :name))
                          (core/model-element? me) (= me %)
                          (string? me) (= (% :name) me))) 
                        (core/model-elements))
          (filter #(match-type-fn %)
                  (core/model-elements)))))))


