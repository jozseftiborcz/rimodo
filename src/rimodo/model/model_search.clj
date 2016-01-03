(ns rimodo.model.model-search
  (:require [rimodo.model.core :as core]))

(defn- re-pattern?
  [s]
  (= (type s) java.util.regex.Pattern))

(defn- me-search-filter
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
        (set? fcond) (some #((me-search-filter %) me) fcond)
        (keyword? fcond) (= (name (me :model-type)) (name fcond))))))

(defn- do-search 
  [result-seq search-filter conds]
  (if (empty? conds)
    result-seq
      (recur (filter (search-filter (first conds)) result-seq) search-filter (rest conds))))


(defn search
  "It filters model elements by conditions as provided. 
  A condition is either a keyword, a string or regexp, hash-map or an array/set. Each condition is applied 
  sequentially each one is narrowing the (model element) result set.
  * keyword is treated as model-element-type specifier.
  * string or regexp filter model elements by name.
  * hash-map filters model elements with attributes/values as specified by key/value pairs.
  * array/set is treated as a grouped filter where one of the group member should match model element."
  [ & conds]
  (with-meta (do-search (lazy-seq (core/model-elements)) me-search-filter conds)
             {:model-search-result true :conds conds}))

(defn- iv-search-filter
  [fcond]
  (fn[[[invariant-type violating-object] violation-infos :as iv]]
    (let [object2str (fn[o] (or 
                              (and (core/model-element? o)
                                   (o :name)) 
                              (str o)))]
      (cond 
        (re-pattern? fcond) (re-find fcond (object2str violating-object))
        (core/model-element? fcond) (= fcond violating-object)
        (string? fcond) (= (object2str violating-object) fcond)
        (set? fcond) (some #((iv-search-filter %) iv) fcond)
        (keyword? fcond) (= invariant-type fcond)))))

(defn iv-search
  "It filters invariant-violations by conditions as provided. 
  A condition is either a keyword, a string or regexp, hash-map or an array/set. Each condition is applied 
  sequentially each one is narrowing the (model element) result set.
  * keyword is treated as violation category.
  * string or regexp filter model elements by name.
  * array/set is treated as a grouped filter where one of the group member should match model element."
  [ & conds]
  (with-meta (do-search (lazy-seq @core/invariant-violations) iv-search-filter conds)
             {:invariant-violations-search-result true :conds conds}))
