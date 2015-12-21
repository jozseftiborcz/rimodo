(ns rimodo.model.server
  (:require [rimodo.model.core :as core]))

;; global state of configuration settings
(def ^:dynamic *config* (atom {:model-print "simple"}))

(defn config 
  "Returns or sets global configuration of the system."
  [& args]
  (cond 
    (= (first args) :model-print) (if (second args) 
                                    (if (= "?" (second args))
                                      (println "simple|verbose")
                                      (swap! *config* assoc :model-print (second args))) (@*config* :model-print))
    :else @*config*))

(defn get-config 
  "Returns global config value"
  [param]
  (config param))

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
         (if (> (count args) 1)
            (swap! model-state assoc (first args) (rest args)) 
            (@model-state (first args))))))

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
      (apply cmd params))))

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
  (case (get-config :model-print)
    "simple" (print-simple (o) writer)
    "verbose" (print-simple (meta o) writer)))

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
                                mef-dump-state
                                mef-print-name))
              (apply-all [~element-sym# ~'model-state# '~attrs#] ~~@(filter meph-filter-fn clauses#))
              ;; pretty printer for REPL
              (defmethod print-method (type ~element-sym#) [~'this# ~'writer#]
                (-model-element-printer #'~(symbol element-name#) ~'writer#))
              ;; add model to global me list
              (core/register-me #'~element-sym#))))))

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

(model-element-type :server)
(model-element-type :application)

(defn find-me
  "This finds a model element by name or symbol. If model element is given it just returns it"
  [me]
  (if (symbol? me) (resolve me) me))
    
(with-meta-defn meph-cluster-member-handler
  {:model-element-param-handler true
   :doc "returns string representation of model element"
   :examples ["(me) returns its string rep."]}
  [element-sym model-state args]
;  (println "meph-cluster-member-handler" element-sym)
  (let [[kv-args rest-args] (separate-kv-attrs args)
        servers (filter server? rest-args)]
;    (if (first rest-args) (println "x0" (type (first rest-args))))
;    (println "x1" (type element-sym) servers rest-args)
    (apply element-sym :cluster-members servers)
    (doseq [server servers] 
      ((find-me server) (keyword "cluster-member") element-sym))))

(model-element-type :cluster-group meph-cluster-member-handler)

(defn runs-on [& args] (cons :runs-on args))

(defn load-model 
  "This function loads a model from a file into namespace model or to the namespace given."
  ([filename]
   (load-model "model" filename))
  ([model filename]
   (let [nsname (ns-name *ns*)
         model (symbol model)]
     (remove-ns model)
     (core/reset-registers!)
     (in-ns model)
     (clojure.core/require '[rimodo.model.server :refer :all] 
                           '[clojure.core :refer :all] 
                           '[rimodo.model.model-search :refer :all] :reload)
     (load-file filename)
     (in-ns nsname))))
