(ns interpreter-environment)

(def the-empty-environment [])

(defn first-frame [env]
  (peek env))

(defn first-frame-id [env]
  (:id (first-frame env)))

(defn modify-frame [var value env frame-id]
  (assoc-in env [frame-id :bindings (keyword var)] value))

(defn enclosing-environment [env]
  (pop env))

(defn make-frame [id vars values]
  (let [bindings (zipmap vars values)]
    {:id id :bindings bindings}))

(defn extend-environment [vars values env]
 (conj env (make-frame (count env) vars values)))

(defn get-var-in-frame [var frame]
  (get-in frame [:bindings (keyword var)]))
