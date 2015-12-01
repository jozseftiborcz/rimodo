(ns rimodo.model.server)

(defmacro server 
  [server-name & attrs]
  (let [s-name (if (symbol? server-name) server-name (symbol server-name))]
    `(def ~s-name (hash-map ~@attrs))))

(defmacro application
  [app-name & attrs]
  (let [a-name (if (symbol? app-name) app-name (symbol app-name))
        attrs (if (odd? (count attrs)) (cons :descr attrs) attrs)
        args '[& args]]
    `(do 
       (defn ~a-name ~args (hash-map ~@attrs)))))

(defn runs-on [& args] (cons :runs-on args))

(defmacro server-group 
  [group-name & servers]
  nil)
