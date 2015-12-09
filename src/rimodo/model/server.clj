(ns rimodo.model.server)

(defn me-attr-setting
  [element-name model-state args]
  ;(println (meta (resolve element-name)))
  (if (keyword? (first args)) 
         (if (> (count args) 1)
            (swap! model-state assoc (first args) (rest args)) 
            (@model-state (first args)))))

(defn me-print-name
  [element-name model-state args]
  (str (@model-state :model-type) " " element-name))

(defmacro functionize [macro] 
  `(fn [ & args#] 
  ;   (println "xx" (count args#) args#)
     (eval (cons '~macro args#))))

(defmacro eval-while-nil 
  [ & conds]
  (when conds
    (list 'clojure.core/if-let ['f (first conds)]
          'f
          (cons 'rimodo.model.server/eval-while-nil (next conds)))))


(defmacro create-model-element
  [model-element-name# & clauses#]
  `(defmacro ~model-element-name# 
     [element-name# & attrs#]
     (let [men# '~model-element-name#
           m-attrs# attrs# ; I dont' know why...
           element-sym# (symbol (name element-name#))] 
       (println element-sym#)
       `(do (let [~'model-state# (atom (apply hash-map '~m-attrs#))]
          (swap! ~'model-state# assoc :model-type '~men#) 
          (defn ~element-sym# [~'& ~'params#]
            (eval-while-nil
              (me-attr-setting '~element-name# ~'model-state# ~'params#)
              (me-print-name '~element-name# ~'model-state# ~'params#))))))))

(defmacro create-model-element-x
  [model-element-name# & clauses#]
  `(defmacro ~model-element-name# 
     [element-name# ~'& attrs#]
     (let [men# '~model-element-name#]
       `(let [~'model-state# (atom (apply hash-map ~attrs#))]
          (defn ~element-name# [~'& ~'params#]
            (if-let [~'res# (me-attr-setting '~element-name# ~'model-state# ~'params#)]
              ~'res#
              (str '~men# " " '~element-name#)))))))

(defmacro model-element
  [model-element-name & clauses]
  (let [model-element-sym (symbol (name model-element-name))]
    (if (resolve model-element-sym) 
      (do 
        (println "WARN: model element type" model-element-sym "will be overwritten!") 
        (ns-unmap *ns* model-element-sym)))
      `(create-model-element ~model-element-sym ~clauses)))

(defmacro model-element-x
  "This function implements the general element defining interface which can be parametrized with extra conditional logic"
  [model-element-name & clauses]
  `(do 
     (ns-unmap *ns* (symbol (name ~model-element-name)))
     (let [model-element-sym# (symbol (name ~model-element-name))]
       (defmacro ~(symbol (name model-element-name))
         [element-name# & attrs#]
         (let [e-name# (if (symbol? element-name#) element-name# (symbol element-name#))
               attrs# (if (odd? (count attrs#)) (cons :descr attrs#) attrs#)]
           `(let [~'model-state# (atom {})]
              (defn ~e-name# [& ~'args#] 
;                (cond 
;           (keyword? (first ~'args#))
;             (if (> (count ~'args#) 1) (swap! ~'model-state# assoc (first ~'args#) (rest ~'args#)) (@~'model-state# (first ~'args#)))
;           (seq? (first ~'args#)) (apply ~e-name# (first ~'args#))
;                           :else (str '~model-element-sym# " " ~(str e-name#))))))))))


                (apply (functionize cond) 
                       (flatten 
                         (list 
;                           ('me-attr-setting ~e-name# ~'model-state# ~'args#)
                           :else (str '~model-element-sym# " " ~(str e-name#))))))))))))
;                (apply (functionize cond) (flatten (list 
 ;                          ('me-attr-setting ~e-name# ~'model-state# ~'args#)

(model-element :server)

(defmacro application
  [app-name & attrs]
  (let [a-name (if (symbol? app-name) app-name (symbol app-name))
        s-name (str app-name)
        attrs (if (odd? (count attrs)) (cons :descr attrs) attrs)

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
    (remove-ns 'model)
    (in-ns 'model)
    (clojure.core/require '[rimodo.model.server :refer :all] :reload)
    (load-file filename)
    (in-ns nsname)))
