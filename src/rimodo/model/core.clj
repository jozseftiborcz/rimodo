(ns rimodo.model.core
  (:require [clojure.pprint :as pp]
            [clojure.string :as s]))

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

;; current model
(def ^:dynamic *model* nil)

(def ^:dynamic ^:private models (atom {}))

(defn set-model
  "This sets the current model to model"
  [m]
  (def ^:dynamic *model* m))

(defn find-model
  "This finds a model by name or model"
  [model-or-name]
  (cond
   (string? model-or-name) (@models model-or-name)
   (and (map? model-or-name) 
        (model-or-name :model-name)) (@models (model-or-name :model-name))))

(defn create-model
  "This creates a new model or returns the existing one"
  [model-name]
  (if-let [model (find-model model-name)]
    model
    (let [new-model {:model-name model-name}]
      (swap! models assoc model-name new-model)
      new-model)))

(defn remove-model
  "This removes the given model"
  [model-or-name]
  (if-let [model (find-model model-or-name)]
    (do (swap! models dissoc (model :model-name)) 
      model)))

(defn all-models
  "Returns a lazy-seq of available models"
  []
  (lazy-seq @models))

(defmacro with-model
  "This executes the body with the given model"
  [model-or-name & body]
  `(if-let [model# (find-model ~model-or-name)]
     (binding [*model* model#]
       ~@body)))

;; global list of model elements
(def -model-elements-register (atom []))
(def -model-types-register (atom []))

(defn register-me!
  [me]
;  (println "register-me" me)
  (swap! -model-elements-register conj me)
  me)

(defn register-mt!
  [mt]
  (swap! -model-types-register conj mt))

(defn model-elements
  "This returns a lazy-seq of model elements"
  []
  (lazy-seq @-model-elements-register))

(defn model-types
  "This returns a lazy-seq of model types."
  []
  (lazy-seq @-model-types-register))

(def invariant-violations (atom {}))

(defn set-violations!
  "This sets invariant violations of a model element by invariant type to violation-infos. 
  violation-info is a string representation of the violation found.
  It is assumed that violation-infos are a full list. So empty list means violations resovled."
  [invariant-type violating-object & violation-infos] 
  (if (empty? violation-infos) 
    (swap! invariant-violations dissoc (list invariant-type violating-object))
    (swap! invariant-violations assoc (list invariant-type violating-object) violation-infos)))

(defmacro model-invariant
  "This defines a model invariant"
  [iname & body]
  `(defn ~iname ~@body))

(defn pattern? [x] (= (type x) java.util.regex.Pattern))

(defn model-type?
  [f]
  (cond
    (var? f) (get (meta f) :model-type)
    (fn? f) false
    (pattern? f) false
    (or (string? f) (keyword? f)) (first (filter #(=((meta %) :name) (symbol (name f))) (model-types)))
    :else false))

(defn model-element?
  "This returns true if the given var is a model element"
  [f] 
  (and (fn? f) (some #(= % f) (model-elements))))

(defn find-me
  "This finds a model element by name or symbol. If model element is given it just returns it"
  [me]
  (if (symbol? me) (resolve me) me))

(defmulti textualize-result class)

(defmethod textualize-result clojure.lang.LazySeq
  [s]
  (with-out-str 
    (binding [*out* (pp/get-pretty-writer *out*)] 
      (pp/with-pprint-dispatch pp/code-dispatch 
        (pp/pprint-logical-block :prefix "  (" :suffix ")"
;          (pp/pprint-indent :block 2) 
          ((pp/formatter-out "~{~s~^~:@_~}") s)) 
          (.ppflush *out*)))))

(defmethod textualize-result nil
  [result]
  (with-out-str (pp/pprint result)))

(defmacro textualize
  "This function output the result of the first parameter into a str with the original definition"
  [tname f & result]
  (let [result2 (eval f)
        filename (s/replace *file* #".clj$" (str "_" tname ".te"))]
    (with-open [w (clojure.java.io/writer filename)]
      (.write w (str "(textualize \"" tname "\" " f "\n"))
      (.write w (str "  " (meta &form) "\n"))
      (.write w (str "  :result\n"))
      (.write w (textualize-result result2))
      (.write w ")"))))

(defn generate-textualize!
  [])

(defn reset-registers!
  []
  (reset! -model-elements-register [])
  (reset! -model-types-register [])
  (reset! invariant-violations {})
  (doall (map #(remove-ns (symbol (str %))) (filter #(re-find #"^model." (str %)) (all-ns)))))

(defn load-model 
  "This function loads a model from a file into namespace model or to the namespace given."
  ([filename]
   (load-model "model" filename))
  ([model filename]
   (let [nsname (ns-name *ns*)
         model (symbol model)]
     (remove-ns model)
     (reset-registers!)
     (in-ns model)
     (clojure.core/require '[rimodo.model.server :refer :all] 
                           '[rimodo.model.core :refer :all]
                           '[clojure.core :refer :all] 
                           '[rimodo.model.model-search :refer :all] :reload)
     (load-file filename)
     (generate-textualize!)
     (in-ns nsname))))

