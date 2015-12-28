(ns rimodo.model.model-search
  (:require [rimodo.model.core :as core]))

(defn- re-pattern?
  [s]
  (= (type s) java.util.regex.Pattern))

(defn- search-filter
  [fcond]
  (fn[me]
    (let [map-eq-attr-fn (fn[attrs me]
                           (= (count attrs) 
                              (count (take-while #(= (me %) (attrs %)) (keys attrs)))))]
      (cond 
        (re-pattern? fcond) (re-find fcond (me :name))
        (core/model-element? fcond) (= fcond me)
        (string? fcond) (= (me :name) fcond)
        (map? fcond) (map-eq-attr-fn fcond me) 
        (set? fcond) (some #((search-filter %) me) fcond)
        (keyword? fcond) (= (name (me :model-type)) (name fcond))))))

(defn- do-search 
  [result-seq conds]
  (declare search-filter)
  (if (empty? conds)
    result-seq
      (recur (filter (search-filter (first conds)) result-seq) (rest conds))))

;          extract-type-fn (fn[x] (x :model-type))
;      (if (core/model-type? (first conds))
;        (if-let [me (second conds)]
;          (filter #(and (match-type-fn %)
;                        (cond 
;                          (re-pattern? me) (re-find me (% :name))
;                          (core/model-element? me) (= me %)
;                          (string? me) (= (% :name) me))) 
;                        (core/model-elements))
;          (filter #(match-type-fn %)
;                  (core/model-elements)))))))
;          match-type-fn (fn[x] (= (symbol (name (extract-type-fn x))) 
;                                  (symbol (name (first conds)))))]

(defn search
  "It filters model elements by conditions as provided. 
  A condition is either a keyword, a string or regexp, hash-map or an array/set. Each condition is applied 
  sequentially each one is narrowing the (model element) result set.
  * keyword is treated as model-element-type specifier.
  * string or regexp filter model elements by name.
  * hash-map filters model elements with attributes/values as specified by key/value pairs.
  * array/set is treated as a grouped filter where one of the group member should match model element."
  [ & conds]
  (do-search (lazy-seq (core/model-elements)) conds))
