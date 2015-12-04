(ns rimodo.model.server)

(defmacro server 
  [server-name & attrs]
  (let [s-name (if (symbol? server-name) server-name (symbol server-name))]
    `(def ~s-name (hash-map ~@attrs))))

(defmacro application
  [app-name & attrs]
  (let [a-name (if (symbol? app-name) app-name (symbol app-name))
        s-name (str app-name)
        attrs (if (odd? (count attrs)) (cons :descr attrs) attrs)
        args '[& args]
        nsname (ns-name *ns*)]
    `(let [model-state# (atom {})] 
;;       (in-ns (quote ~'app-ns))
       (defn ~a-name [& args#]
         (cond
           (keyword? (first args#)) 
             (if (> (count args#) 1) (swap! model-state# assoc (first args#) (rest args#)) (@model-state# (first args#)))
           (seq? (first args#)) (apply ~a-name (first args#))
           :else (str "application " ~s-name)))
;;       (in-ns (quote ~nsname))
            )))

(defn model-fn 
  [the-model params]
  nil)

(defn runs-on [& args] (cons :runs-on args))

(defmacro server-group 
  [group-name & servers]
  nil)

(defn load-model 
  [filename]
  (let [nsname (ns-name *ns*)]
    (in-ns 'model)
    (clojure.core/require '[rimodo.model.server :refer :all])
    (load-file filename)
    (in-ns nsname)))
