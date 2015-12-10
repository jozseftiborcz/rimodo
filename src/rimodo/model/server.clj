(ns rimodo.model.server)

(defn me-attr-setting
  ^{:model-element-function true
    :doc "attribute get/set"
    :examples ["(me attr1-kwd attr1-value attr2-kwd attr2-value) sets attr1 and attr2"
               "(me attr1-kwd) returns value of attr1-value or nil"]}
  [element-name model-state args]
  ;(println (meta (resolve element-name)))
  (if (keyword? (first args)) 
         (if (> (count args) 1)
            (swap! model-state assoc (first args) (rest args)) 
            (@model-state (first args)))))

(defn me-print-name
  ^{:model-element-function true
    :doc "returns string representation of model element"
    :examples ["(me) returns its string rep."]}
  [element-name model-state args]
  (str (@model-state :model-type) " " element-name))

(defmacro functionize [macro] 
  `(fn [ & args#] 
  ;   (println "xx" (count args#) args#)
     (eval (cons '~macro args#))))

(defmacro eval-while-nil 
  "This macro generates code which evaluates conds until one returns not nil value."
  [ & conds]
  (when conds
    (list 'clojure.core/if-let ['f (first conds)]
          'f
          (cons 'rimodo.model.server/eval-while-nil (next conds)))))

(defmacro create-model-element
  "This generates model element with model-element-name. Clauses define additional behaviour to generated model element functionality. The macro is used internally."
  [model-element-name# & clauses#]
  `(defmacro ~model-element-name# 
     [element-name# & attrs#]
     (let [men# '~model-element-name#
           ; I don't know why it needs new variable (m-attrs) in order to reference in the nested
           ; syntax quoting scope. 
           m-attrs# (if (odd? (count attrs#)) (cons :descr attrs#) attrs#) 
           element-sym# (symbol (name element-name#))] 
       `(do (let [~'model-state# (atom (apply hash-map '~m-attrs#))]
          (swap! ~'model-state# assoc :model-type '~men#) 
          (defn ~element-sym# [~'& ~'params#]
            (eval-while-nil
              (me-attr-setting '~element-name# ~'model-state# ~'params#)
              (me-print-name '~element-name# ~'model-state# ~'params#))))))))

(defmacro model-element
  ^{:doc "This generates model element with model-element-name. Clauses define additional behaviour to generated model element functionality. If the name is used in the namespace it will be overwritten."
    :examples ["(model-element :foo) defines a model element type foo, which can be used to define model element bar as (foo bar)"]}
  [model-element-name & clauses]
  (let [model-element-sym (symbol (name model-element-name))]
    (if (resolve model-element-sym) 
      (do 
        (println "WARN: model element type" model-element-sym "will be overwritten!") 
        (ns-unmap *ns* model-element-sym)))
      `(create-model-element ~model-element-sym ~clauses)))

(model-element :server)
(model-element :application)

(defn model-fn 
  [the-model params]
  nil)

(defn runs-on [& args] (cons :runs-on args))

(defmacro server-group 
  [group-name & servers]
  nil)

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
