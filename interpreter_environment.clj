(ns interpreter-environment)

;; an environment is a map of :vars values and a special key
;; 'next whose value is the enclosing environment. The 'next key
;; of the global environment points to the empty environment
(def the-empty-environment nil)
(def ^:private next-env 'next)

(defn modify! [var value *env*]
  (swap! *env* #(assoc %1 (keyword var) value)))

(defn enclosing-environment [env]
  ('next env))

(defn make-new [vars values *base-env*]
  (-> (zipmap vars values)
      (assoc next-env *base-env*)
      (atom)))

(defn var-value [var env]
  ((keyword var) env))
