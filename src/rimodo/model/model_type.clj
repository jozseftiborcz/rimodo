(ns rimodo.model.model-type
  (:require [rimodo.model.core :as core]))

(defmacro with-meta-defn 
  [fn-name fn-metadata args & body]
  `(defn ~(with-meta fn-name fn-metadata) ~args ~@body))

(def mef-attr-setting
  ^{:model-element-function true
    :doc "attribute get/set"
    :examples ["(me attr1-kwd attr1-value attr2-kwd attr2-value) sets attr1 and attr2"
               "(me attr1-kwd) returns value of attr1-value or nil"]}
  (fn [element-name model-state args]
    ;(println (meta (resolve element-name)))
    (if (keyword? (first args)) 
      (case (count args)
        1 (@model-state (first args))
        2 (swap! model-state assoc (first args)  (second args))
        (swap! model-state assoc (first args) (rest args))))))

(def mef-print-name
  ^{:model-element-function true
    :doc "returns string representation of model element"
    :examples ["(me) returns its string rep."]}
  (fn [element-sym model-state args]
    (str (name (@model-state :model-type)) " " (str (@model-state :name)))))

(with-meta-defn mef-dump-state
  {:model-element-function true
   :doc "returns element state"
   :examples ["(me :dump-state) returns its state."]}
  [element-sym model-state args]
  (if (= (first args) :dump-state) @model-state))

(defmacro functionize [macro] 
  `(fn [ & args#] 
  ;   (println "xx" (count args#) args#)
     (eval (cons '~macro args#))))

(defn eval-while-nil
  [params & commands]
;  (println commands)
  (when commands
    (let [cmd (first commands)
          res (if cmd (apply cmd params))]
      (if res res (apply eval-while-nil params (next commands))))))

(defn apply-all
  [params & commands]
;  (println "apply-all" commands)
  (when commands
    (doseq [cmd commands]
      (if cmd (apply cmd params)))))

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
  "This function splits inputs into two: a hash-map of key-value pairs and the rest. 
  KV pairs are identified by symbol keys."
  ([args]
   (if (and (keyword? (first args)) (next args))
     (separate-kv-attrs (apply hash-map (take 2 args)) (drop 2 args))
     args))
  ([kv-args args]
   (if (and (keyword? (first args)) (next args))
     (recur (assoc kv-args (first args) (second args)) (drop 2 args))
     (list kv-args args))))

(defn -model-element-printer 
  [o writer]
  (case (core/get-config :model-print)
    "simple" (print-simple (o) writer)
    "verbose" (print-simple (meta o) writer)))

; this variable holds global injected behaviours
(def injected-behaviour-store (atom {:global []}))

(defn reset-mt!
  []
  (reset! injected-behaviour-store {:global []}))

(defn inject-behaviour 
  "It injects behaviour to model element type. Inject dest can be :global model element type keyword or model element."
  ([inject-dest b-name behaviour-fn] 
  (if-not (@injected-behaviour-store inject-dest) (swap! injected-behaviour-store assoc inject-dest []))
  (swap! injected-behaviour-store 
         assoc-in 
         [inject-dest] 
         ;todo here wrap it into an fn to return b-name if called without parameter. 
         ;todo b-fn should accept a hashmap instead.
         (cons behaviour-fn (get-in @injected-behaviour-store [inject-dest]))))
  ([inject-dest behaviour-fn]
   (inject-behaviour inject-dest nil behaviour-fn)))

(defn injected-behaviours
  "This returns a list of behaviours injected to inject-dest"
  [inject-dest]
;  (println "ssss" @injected-behaviour-store (get-in @injected-behaviour-store [inject-dest]))
  (get-in @injected-behaviour-store [inject-dest]))

(defmacro -create-model-type
  "This generates model element with model-type-name. Internal use only, external interface is model-element.
  Clauses define additional behaviour to generated model element functionality. 
  The macro is used internally."
  [model-type-name# & clauses#]
;  (if (first clauses#) (println "zzzz" (meta (resolve (first clauses#))) (first clauses#)))
  `(defmacro ~model-type-name# 
     [element-name# & attrs#]
     (let [men# '~model-type-name#
           ; I don't know why it needs new variable (m-attrs) in order to reference in the nested
           ; syntax quoting scope. 
           attrs# (if (odd? (count attrs#)) (cons :descr attrs#) attrs#) 
           [kv-attrs# rest-attrs#] (separate-kv-attrs attrs#)
           element-sym# (with-meta (symbol (name element-name#)) 
                                   (hash-map :model-element true 
                                             :model-type #'~model-type-name#
                                             :name (name element-name#)))]
       `(do (let [~'model-state# (atom '~kv-attrs#)]
              (swap! ~'model-state# assoc :model-type (keyword '~men#) :name (name '~element-name#)) 
;;              (map #(fn[~'x#] (apply ~'x# (range 5))) ~~@(filter meph-filter-fn clauses#))
              ;; model element as composed function 
              (defn ~element-sym# [~'& ~'params#]
                (eval-while-nil [~element-sym# ~'model-state# ~'params#] 
                                ~~@(filter mef-filter-fn clauses#) 
                                mef-attr-setting 
                                (injected-behaviours ~element-sym#) 
                                mef-dump-state
                                mef-print-name))
              ; add model to global me list
              ; here instead of adding var the model element is added. In the future it may be necessary to 
              ; provide facility to query source structre: eg. where a certain model element is used.
              (core/register-me! ~element-sym#)
              ;(intern (in-ns (symbol (str "model." '~men#))) '~element-sym# ~element-sym#)
              (apply apply-all [~element-sym# '~element-sym# '~men# ~'model-state# '~attrs#] (injected-behaviours :global))
              (apply apply-all [~element-sym# ~'model-state# '~attrs#] (injected-behaviours (keyword '~men#)))
              (apply-all [~element-sym# ~'model-state# '~attrs#] ~~@(filter meph-filter-fn clauses#))
              ;; pretty printer for REPL
              (defmethod print-method (type ~element-sym#) [~'this# ~'writer#]
                (-model-element-printer #'~(symbol element-name#) ~'writer#)))))))

(defmacro model-element-type
  ^{:doc "This generates model element with model-element-name. 
         Clauses define additional behaviour to generated model element functionality. 
         If the name is used in the namespace it will be overwritten."
    :examples ["(model-element :foo) defines a model element type foo, which can be used 
               to define model element bar as (foo bar)"]}
  [model-type-name & clauses]
  (let [model-type-sym (with-meta (symbol (name model-type-name))
                         (hash-map :model-type true))]
    (if (resolve model-type-sym) 
      (do 
        (println "WARN: model element type" model-type-sym "will be overwritten!") 
        (ns-unmap *ns* model-type-sym)))
    `(do 
       (-create-model-type ~model-type-sym ~@clauses)
       (core/register-mt #'~model-type-sym)
       (defn ~(symbol (str model-type-sym "?"))
         "It decides if a model element is of this type. Returns false if not a model element."
         [me#]
         (and (fn? me#) (= ((me# :dump-state) :model-type) ~model-type-name))))))

(defn runs-on [& args] (cons :runs-on args))

(inject-behaviour :global "unique name handler"
  (fn[me element-sym model-type model-state attrs]
    ; create model element under model element type namespace
    (let [model-ns (create-ns (symbol (str "model." model-type)))]
      (if (ns-resolve model-ns element-sym)
        (core/set-violations! :unique-model-element-name me (str "Element name " element-sym " is not unique"))
        (intern model-ns element-sym me)))))


