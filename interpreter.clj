(ns interpreter
  (:require [default-syntax :as s]
            [interpreter-environment :as e]))

(defmacro invariant [& exps]
  `{:pre [~@exps]
    :post [~@exps]})

(defn -dispatch [exp]
  (cond (s/self-evaluating? exp) :to-self
        (s/variable? exp) :variable
        (s/a-list? exp) (if-let [form (s/special-form? exp)]
                          form
                          :application)
        :else (throw (Exception. (str "-dispatch / Unknown form " exp)))))

(defmulti eval-exp (fn [[exp env]] (-dispatch exp)))

(defmethod eval-exp :to-self [[exp env]]
  [exp env])

(defn lookup-variable-value [var env]
    (if (= env e/the-empty-environment)
      (throw (Exception. (str "Unbound variable " var)))
      (if-let [value (e/get-var-in-frame var (e/first-frame env))]
          value
          (recur var (e/enclosing-environment env)))))

(defmethod eval-exp :variable [[exp env]]
  [(lookup-variable-value exp env) env])

(defmethod eval-exp :quotation [[exp env]]
  [(s/text-of-quotation exp) env])

(defn set-variable-value [var value env]
  (loop [var var, value value, env env, rest-env env]
    (cond (= rest-env e/the-empty-environment)
            (throw (Exception. (str "Unbound variable " var)))
          (e/get-var-in-frame var (e/first-frame rest-env))
            (e/modify-frame var value env (e/first-frame-id rest-env))
          :else (recur var value env (e/enclosing-environment rest-env)))))

(defmethod eval-exp :assignment [[exp env]]
  (let [var (s/assignment-variable exp)
        value (s/assignment-value exp)
        [value env] (eval-exp [value env])
        env (set-variable-value var value env)]
    [(str var " : " value) env]))

(defn define-variable [var value env]
  (e/modify-frame var value env (e/first-frame-id env)))

(defmethod eval-exp :definition [[exp env]]
  (let [var (s/definition-variable exp)
        value (s/definition-value exp)
        [value env] (eval-exp [value env])
        env (define-variable var value env)]
    [(str var " : " value) env]))

(defn -true? [x]
  (not (= false x)))

(defn -false? [x]
  (= false x))

(defmethod eval-exp :if [[exp env]]
  (let [[pred env] (eval-exp [(s/if-predicate exp) env])]
    (if (-true? pred)
      (eval-exp [(s/if-consequent exp) env])
      (eval-exp [(s/if-alternative exp) env]))))

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))

(defmethod eval-exp :procedure [[exp env]]
  [(make-procedure (s/lambda-params exp) (s/lambda-body exp) env) env])

(defn eval-sequence [actions env]
  (if (seq actions)
    (loop [[ret env] (eval-exp [(first actions) env])
           rest-actions (rest actions)]
      (if (empty? rest-actions)
        [ret env]
        (recur (eval-exp [(first rest-actions) env]) (rest rest-actions))))
    nil))

(defmethod eval-exp :list-of-actions [[exp env]]
  (eval-sequence) (s/actions exp) env)

(defmethod eval-exp :cond [[exp env]]
  (eval-exp [(s/cond->if exp) env]))

(defmethod eval-exp :or [[exp env]]
  (loop [exp (s/or-clauses exp)
         env env]
    (if (seq exp)
      (if-let [x (eval-exp [(first exp) env])]
        x
        (recur (next exp) env))
      false)))

(defmethod eval-exp :and [[exp env]]
  (loop [res true exp (s/and-clauses exp) env env]
    (if (seq exp)
      (if-let [x (eval-exp [(first exp) env])]
        (recur x (next exp) env)
        false)
      res)))

(defmethod eval-exp :let [[exp env]]
  ;; (println (s/let->combination exp)))
  (eval-exp [(s/let->combination exp) env]))

(defmethod eval-exp :let* [[exp env]]
  ;; (println (s/let*->nested-lets exp)))
  (eval-exp [(s/let*->nested-lets exp) env]))

(defmethod eval-exp :while [[exp env]]
  ;; (println (s/while->lambda exp)))
  (eval-exp [(s/while->lambda exp) env]))

(defn list-of-values-left-eval-exp [[exps env]]
  (if (s/no-operands? exps)
    ()
    (let [first-exp (eval-exp [(s/first-operand exps) env])]
      (conj (list-of-values-left-eval-exp (s/rest-operands exps) env) first-exp))))

(defn list-of-values-right-eval-exp [[exps env]]
  (if (s/no-operands? exps)
    ()
    (let [tail-exps (list-of-values-right-eval-exp (s/rest-operands exps) env)]
      (conj tail-exps (eval-exp [(s/first-operand exps) env])))))

(def list-of-values list-of-values-left-eval-exp)

(defn compound-procedure? [p]
  (and (list? p) (= 'procedure (first p))))

(defn procedure-parameters [p] (second p))
(defn procedure-body [p] (nth p 2))
(defn procedure-environment [p] (last p))

(defn primitive-procedure? [proc]
  (= 'primitive (first proc)))

(defn primitive-implementation [proc]
  (second proc))

(defn apply-primitive-procedure [proc args]
  (apply (primitive-implementation proc) args))

(defn -apply [procedure arguments]
  (cond (primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
           (eval-sequence
            (procedure-body procedure)
            (e/extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure)))
        :else (throw (Exception. (str "Unknown procedure type - APPLY" procedure)))))

(defmethod eval-exp :application [[exp env]]
  (-apply (eval-exp (s/operator exp) env)
         (list-of-values (s/operands exp) env)))

;; (defn user-print [object]
;;   (if (compound-procedure? object)
;;     (print (list 'compound-procedure
;;                     (procedure-parameters object)
;;                     (procedure-body object)
;;                     '<procedure-env>))
;;     (print object)))

(def the-global-environment e/the-empty-environment)

(defn update-the-global-environment! [env]
  (alter-var-root #'the-global-environment (fn [old] env)))

(defn setup-environment! []
  (update-the-global-environment!
   (e/extend-environment (s/primitive-procedure-names)
                         (s/primitive-procedure-objects)
                         e/the-empty-environment)))

(defn scheme-eval-exp [input]
  (when (= e/the-empty-environment the-global-environment)
    (setup-environment!))
  (let [[output env] (eval-exp [input the-global-environment])]
    (println (str "\n out --> " output))
    (update-the-global-environment! env)
    (newline)))

;; convenience aliases for REPL
(def e scheme-eval-exp)
(def g the-global-environment)
