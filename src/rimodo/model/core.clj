(ns rimodo.model.core)

;; global list of model elements
(def -model-elements-register (atom []))
(def -model-types-register (atom []))

(defn register-me
  [me]
  (swap! -model-elements-register conj me))

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

(defn model-type?
  [o]
  (some #(=((meta %) :name) (symbol (name o))) (model-types)))

