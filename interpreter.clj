(ns interpreter
  (:require [default-syntax :as s]))

(def default-env {})

(declare -eval)

(defn -dispatch [exp]
  (cond (s/self-evaluating? exp) :to-self
        (s/variable? exp) :variable
        (s/a-list? exp) (if-let [form (s/special-form? exp)]
                          form
                          :application)
        :else (throw (Exception. (str "-dispatch / Unknown form " exp)))))

(defmulti eval-exp (fn [exp env] (-dispatch exp)))

(defn -eval
  ([exp] (-eval exp default-env))
  ([exp env] (eval-exp exp env)))

(defmethod eval-exp :to-self [exp env]
  exp)

(declare lookup-variable-value)
(defmethod eval-exp :variable [exp env]
  (lookup-variable-value exp env))

(defmethod eval-exp :quotation [exp env]
  (s/text-of-quotation exp))

(declare set-variable-value!)
(defmethod eval-exp :assignment [exp env]
  (set-variable-value! (key (s/assignment-variable exp))
                       (-eval (s/assignment-value exp env))
                       env))

(declare define-variable!)
(defmethod eval-exp :definition [exp env]
  (define-variable! (key (s/definition-variable exp))
    (-eval (s/definition-value) env)
    env))

(defn -true? [x]
  (not (= false x)))

(defn -false? [x]
  (= false x))

(defmethod eval-exp :if [exp env]
  (if (-true? (-eval (s/if-predicate exp) env))
    (-eval (s/if-consequent exp) env)
    (-eval (s/if-alternative exp) env)))

(declare make-procedure)
(defmethod eval-exp :procedure [exp env]
  (make-procedure (s/lambda-params exp) (s/lambda-body exp) env))

(defn eval-sequence [exp env]
  (if (seq exp)
    (do (eval-exp (first exp) env)
        (recur (next exp) env))
    nil))

(defmethod eval-exp :list-of-actions [exp env]
  (eval-sequence) (s/actions exp) env)

(defmethod eval-exp :cond [exp env]
  (-eval (s/cond->if exp) env))

(defmethod eval-exp :or [exp env]
  (loop [exp (s/or-clauses exp)
         env env])
  (if (seq exp)
    (if-let [x (-eval (first exp) env)]
      x
      (recur (next exp) env))
    false))

(defmethod eval-exp :and [exp env]
  (loop [res true exp (s/and-clauses exp) env env]
    (if (seq exp)
      (if-let [x (-eval (first exp) env)]
        (recur x (next exp) env)
        false)
      res)))

(defmethod eval-exp :let [exp env]
  ;; (println (s/let->combination exp)))
  (-eval (s/let->combination exp) env))

(defmethod eval-exp :let* [exp env]
  ;; (println (s/let*->nested-lets exp)))
  (-eval (s/let*->nested-lets exp) env))

(defmethod eval-exp :while [exp env]
  ;; (println (s/while->lambda exp)))
(-eval (s/while->lambda exp) env))

(defn list-of-values-left-eval [exps env]
  (if (s/no-operands? exps)
    ()
    (let [first-exp (-eval (s/first-operand exps) env)]
      (conj (list-of-values-left-eval (s/rest-operands exps) env) first-exp))))

(defn list-of-values-right-eval [exps env]
  (if (s/no-operands? exps)
    ()
    (let [tail-exps (list-of-values-right-eval (s/rest-operands exps) env)]
      (conj tail-exps (-eval (s/first-operand exps) env)))))

(def list-of-values list-of-values-left-eval)

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))

(defn compound-procedure? [p]
  (= 'procedure (first p)))

(defn procedure-parameters [p] (second p))
(defn procedure-body [p] (nth p 2))
(defn procedure-environment [p] (last p))

;; the environment is a list of frames indentified by their position in the vector
(defn enclosing-environment [env] (next env))
(defn first-frame [env] (first env))
(def the-empty-environment nil)

;; usage: (make-frame [:a 5 :b 10 ...])
(defn make-frame [bindings]
  (apply hash-map bindings))

(defn lookup-variable-value [var env]
  (if (= env the-empty-environment)
    (throw (Exception. (str "Unbound variable " var)))
    (let [value (get var (first-frame env) :not-found)]
      (if (= value :not-found)
        (recur var (enclosing-environment env))
        value))))

(defn extend-environment [bindings base-env]
  (alter-var-root base-env
                  (fn [env] (conj env (make-frame bindings)))))

(defn define-variable! [var value env]
  (alter-var-root env
                  (fn [env] (assoc (first-frame env) var value))))

(defn set-variable-value! [var value env]
  (if (= env the-empty-environment)
    (throw (Exception. (str "Unbound variable " var)))
    (if (contains? (first-frame env) var)
      (alter-var-root env
                      (fn [env] (assoc (first-frame env) var value)))
      (recur var value (enclosing-environment env)))))

(defn apply-primitive-procedure)

(defn -apply [procedure arguments]
  (cond (s/primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
           (eval-sequence
            (procedure-body procedure)
            (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure)))
        :else (error "Unknown procedure type - APPLY" procedure)))

(defmethod eval-exp :application [exp env]
  (-apply (-eval (s/operator exp) env)
         (list-of-values (s/operands exp) env)))
