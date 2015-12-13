(ns rimodo.model.server)

(def mef-attr-setting
  ^{:model-element-function true
    :doc "attribute get/set"
    :examples ["(me attr1-kwd attr1-value attr2-kwd attr2-value) sets attr1 and attr2"
               "(me attr1-kwd) returns value of attr1-value or nil"]}
  (fn [element-name model-state args]
  ;(println (meta (resolve element-name)))
  (if (keyword? (first args)) 
         (if (> (count args) 1)
            (swap! model-state assoc (first args) (rest args)) 
            (@model-state (first args))))))

(def mef-print-name
  ^{:model-element-function true
    :doc "returns string representation of model element"
    :examples ["(me) returns its string rep."]}
  (fn [element-name model-state args]
  (str (@model-state :model-type) " " (name element-name))))

(defmacro functionize [macro] 
  `(fn [ & args#] 
  ;   (println "xx" (count args#) args#)
     (eval (cons '~macro args#))))

(defmacro eval-while-nil-x 
  "This macro generates code which evaluates conds until one returns not nil value."
  [ & conds]
  (when (seq conds)
    (list 'clojure.core/if-let ['f (if (seq? (first conds))
                                     (cons 'rimodo.model.server/eval-while-nil (first conds))
                                     (first conds))]
          'f
          (cons 'rimodo.model.server/eval-while-nil (next conds)))))

(defn eval-while-nil
  [params & commands]
  (when commands
    (let [cmd (first commands)
          res (if cmd (apply cmd params))]
      (if res res (apply eval-while-nil params (next commands))))))

(defmacro call-seq-with-params-x
  [params & clauses]
  (if (and clauses (first clauses))
    `(cons (apply ~(first clauses) ~params) (call-seq-with-params ~params ~@(next clauses))) 
    (list)))

(defn meta-attr-filter-fn
  "It returns a filter fn which returns a filtered seq where meta contains filter-value as key."
  [filter-value]
  (fn[x] 
    (contains? (meta (resolve x)) filter-value)))

(def meph-filter-fn (meta-attr-filter-fn :model-element-param-handler))
(def mef-filter-fn (meta-attr-filter-fn :model-element-function))

(defn separate-kv-attrs
  "This function splits inputs into two: a hash-map of key-value pairs and the rest. KV pairs are identified by symbol keys."
  ([args]
   (if (and (keyword? (first args)) (next args))
     (separate-kv-attrs (apply hash-map (take 2 args)) (drop 2 args))
     args))
  ([kv-args args]
   (if (and (keyword? (first args)) (next args))
     (recur (assoc kv-args (first args) (second args)) (drop 2 args))
     (list kv-args args))))

(defmacro create-model-element
  "This generates model element with model-element-name. Clauses define additional behaviour to generated model element functionality. The macro is used internally."
  [model-element-name# & clauses#]
;  (if (first clauses#) (println "zzzz" (meta (resolve (first clauses#))) (first clauses#)))
  `(defmacro ~model-element-name# 
     [element-name# & attrs#]
     (let [men# '~model-element-name#
           ; I don't know why it needs new variable (m-attrs) in order to reference in the nested
           ; syntax quoting scope. 
           attrs# (if (odd? (count attrs#)) (cons :descr attrs#) attrs#) 
           [kv-attrs# rest-attrs#] (separate-kv-attrs attrs#)
           element-sym# (with-meta (symbol (name element-name#)) 
                                   (hash-map :model-element true :model-type (name '~model-element-name#)))]
       `(do (let [~'model-state# (atom '~kv-attrs#)]
              (swap! ~'model-state# assoc :model-type '~men#) 
              (defn ~element-sym# [~'& ~'params#]
                (eval-while-nil ['~element-sym# ~'model-state# ~'params#] 
                                ~~@(filter meph-filter-fn clauses#) 
                                mef-attr-setting 
                                mef-print-name)))))))

(defmacro model-element
  ^{:doc "This generates model element with model-element-name. Clauses define additional behaviour to generated model element functionality. If the name is used in the namespace it will be overwritten."
    :examples ["(model-element :foo) defines a model element type foo, which can be used to define model element bar as (foo bar)"]}
  [model-element-name & clauses]
  (let [model-element-sym (symbol (name model-element-name))]
    (if (resolve model-element-sym) 
      (do 
        (println "WARN: model element type" model-element-sym "will be overwritten!") 
        (ns-unmap *ns* model-element-sym)))
      `(create-model-element ~model-element-sym ~@clauses)))

(model-element :server)
(model-element :application)

(defmacro with-meta-defn 
  [fn-name fn-metadata args & body]
  `(defn ~(with-meta fn-name fn-metadata) ~args ~@body))

(with-meta-defn meph-cluster-member-handler
  {:model-element-param-handler true
    :doc "returns string representation of model element"
    :examples ["(me) returns its string rep."]}
  [element-name model-state args]
  (if (= (first args) :haha) (str "haha")))

(with-meta-defn meph-cluster-member-handler2
  {:model-element-param-handler true
    :doc "returns string representation of model element"
    :examples ["(me) returns its string rep."]}
  [element-name model-state args]
  (if (= (first args) :huhu) (str "huhu")))

(model-element :cluster-group meph-cluster-member-handler meph-cluster-member-handler2)

(println "xxxx" (meta meph-cluster-member-handler))
(defn model-fn 
  [the-model params]
  nil)

(defn runs-on [& args] (cons :runs-on args))

(defn load-model 
  "This function loads a model from a file into namespace model or to the namespace given."
  ([filename]
   (load-model "model" filename))
  ([model filename]
   (let [nsname (ns-name *ns*)
         model (symbol model)]
     (remove-ns model)
     (in-ns model)
     (clojure.core/require '[rimodo.model.server :refer :all] :reload)
     (load-file filename)
     (in-ns nsname))))
