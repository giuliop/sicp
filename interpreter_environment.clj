(ns interpreter-environment)

;; an environment is a map of :vars values and a special key
;; 'next whose value is the enclosing environment. The 'next key
;; of the global environment points to the empty environment
(def the-empty-environment nil)
(def ^:private next-env 'next)

(defn modify! [var value *env*]
  (swap! *env* #(assoc %1 (keyword var) value)))

(defn enclosing-environment [env]
  (next-env env))

(defn extend-with [vars values *base-env*]
  (-> (zipmap (map keyword vars) values)
      (assoc next-env *base-env*)
      (atom)))

(defn var-value [var env]
  ((keyword var) env '_*unbound*_))

(defn pointer? [env]
  (instance? clojure.lang.Atom env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTING FACILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn env-name [env]
  (if (pointer? env)
    (if (= the-empty-environment (enclosing-environment @env))
      "global env"
      (str env))
    (cond (= env the-empty-environment) "empty env"
          (= the-empty-environment (enclosing-environment env)) "global env"
          :else "env ?")))

(defn env-value [env]
  (if (pointer? env)
    @env
    env))

(defn pprint [env]
  (let [name (env-name env)
        env (env-value env)
        map-f (fn [[key val]]
                (cond (= next-env key) [key (env-name val)]
                      (and (list? val) (= 'procedure (first val)))
                        [key (concat (take 3 val) (list (env-name name)))]
                      :else [key val]))
        primitive? (fn [[key val]] (and (list? val) (= 'primitive (first val))))]
    (println (str name " - " (into {} (map map-f (remove primitive? (seq env))))))))
