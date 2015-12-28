(ns rimodo.model.core)

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

;; global list of model elements
(def -model-elements-register (atom []))
(def -model-types-register (atom []))

(defn register-me
  [me]
  (swap! -model-elements-register conj me)
  me)

(defn register-mt
  [mt]
  (swap! -model-types-register conj mt))

(defn reset-registers!
  []
  (reset! -model-elements-register [])
  (reset! -model-types-register []))

(defn model-elements
  []
  (lazy-seq @-model-elements-register))

(defn model-types
  []
  (lazy-seq @-model-types-register))

(defn pattern? [x] (= (type x) java.util.regex.Pattern))

(defmacro model-type?
  [f]
  (println f)
  (cond
    (symbol? f) `(not(nil? (get (meta #'~f) :model-type)))
    :else `(if (pattern? ~f)
             false
             (some #(=((meta %) :name) (symbol (name ~f))) (model-types)))))

(defmacro model-elementx?
  "This returns true if the given var is a model element"
  [f]
  (if `~(resolve (symbol f)) `(get (meta #'~f) :model-element) false))

(defn model-element?
  "This returns true if the given var is a model element"
  [f]
  (and (fn? f) (try (not(nil? (f :model-type))) (catch clojure.lang.ArityException e))))

(defn find-me
  "This finds a model element by name or symbol. If model element is given it just returns it"
  [me]
  (if (symbol? me) (resolve me) me))

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
                           '[clojure.core :refer :all] 
                           '[rimodo.model.model-search :refer :all] :reload)
     (load-file filename)
     (in-ns nsname))))

