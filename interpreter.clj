(ns interpreter
  (:require [default-syntax :as syn]
            [interpreter-environment :as env]))

(defmacro invariant [& exps]
  `{:pre [~@exps]
    :post [~@exps]})

;; this line is to make sure changes to defmulti are re-evaluated in cider
(def eval-exp nil)

(def the-global-environment)

(defn debug-value-env [value env]
  (println value)
  (env/pprint env)
  (newline))

(defn -dispatch [exp *env*]
  {:pre [(env/pointer? *env*)]}
  ;; (debug-value-env exp *env*)
  (cond (syn/self-evaluating? exp) :to-self
        (syn/variable? exp) :variable
        (syn/a-list? exp) (if-let [form (syn/special-form? exp)]
                          form
                          :application)
        :else (throw (Exception. (str "-dispatch / Unknown form " exp)))))

(defmulti eval-exp -dispatch)

(defmethod eval-exp :to-self [exp *env*]
  exp)

(defn lookup-variable-value [var env]
  (let [value (env/var-value var env)
        next (env/enclosing-environment env)]
    (if (not= value :not-found)
      value
      (if (not= next env/the-empty-environment)
        (recur var @next)
        (throw (Exception. (str "Unbound variable " var)))))))

(defmethod eval-exp :variable [exp *env*] 
  (lookup-variable-value exp @*env*))

(defmethod eval-exp :quotation [exp *env*]
  (syn/text-of-quotation exp))

(defn set-variable-value! [var value *env*]
  (cond (= *env* env/the-empty-environment)
          (throw (Exception. (str "Unbound variable " var)))
        (not= :not-found (env/var-value var @*env*))
          (env/modify! var value *env*)
        :else (recur var value (env/enclosing-environment @*env*))))

(declare user-format)
(defmethod eval-exp :assignment [exp *env*]
  (let [var (syn/assignment-variable exp)
        value (syn/assignment-value exp)
        value (eval-exp value *env*)]
    (set-variable-value! var value *env*)
    (str var " : " (user-format value))))

(defn define-variable! [var value *env*]
  (env/modify! var value *env*))

(declare user-format)
(defmethod eval-exp :definition [exp *env*]
  (let [var (syn/definition-variable exp)
        value (syn/definition-value exp)
        value (eval-exp value *env*)]
    (define-variable! var value *env*)
    (str var " : " (user-format value))))

(defn -true? [x]
  (not (= false x)))

(defn -false? [x]
  (= false x))

(defmethod eval-exp :if [exp *env*]
  (let [pred (eval-exp (syn/if-predicate exp) *env*)]
    (if (-true? pred)
      (eval-exp (syn/if-consequent exp) *env*)
      (eval-exp (syn/if-alternative exp) *env*))))

(defmethod eval-exp :cond [exp *env*]
  (eval-exp (syn/cond->if exp) *env*))

(defmethod eval-exp :or [exp *env*]
  (loop [exps (syn/or-clauses exp)
         *env* *env*]
    (if (seq exps)
      (if-let [x (eval-exp (first exps) *env*)]
        x
        (recur (next exps) *env*))
      false)))

(defmethod eval-exp :and [exp *env*]
  (loop [exp (syn/and-clauses exp)
         *env* *env*
         res true]
    (if (seq exp)
      (if-let [x (eval-exp (first exp) *env*)]
        (recur (next exp) *env* x)
        false)
      res)))

(defmethod eval-exp :let [exp *env*]
  (eval-exp (syn/let->combination exp) *env*))

(defmethod eval-exp :let* [exp env]
  (eval-exp (syn/let*->nested-lets exp) env))

(defmethod eval-exp :while [exp *env*]
  (eval-exp (syn/while->lambda exp) *env*))

(defn list-of-values-left-eval-exp [exps *env*]
  (loop [res () exps exps]
    (if (syn/no-operands? exps)
      res
      (let [value (eval-exp (syn/first-operand exps) *env*)
            res (concat res (list value))
            exps (syn/rest-operands exps)]
        (recur res exps)))))

(defn list-of-values-right-eval-exp [exps *env*]
  (if (syn/no-operands? exps)
    ()
    (let [tail-exps (list-of-values-right-eval-exp (syn/rest-operands exps) *env*)
          exp (eval-exp (syn/first-operand exps) *env*)]
      (conj tail-exps exp) *env*)))

(def list-of-values list-of-values-left-eval-exp)

(defn eval-sequence [actions *env*]
  (let [first-value (eval-exp (syn/first-action actions) *env*)]
    (if (syn/last-action? actions)
      first-value
      (recur (syn/rest-actions actions) *env*))))

(defmethod eval-exp :list-of-actions [exp *env*]
  (eval-sequence (syn/sequence-actions exp) *env*))

(defn scan-out-defines [body]
  (let [definition? #(= :definition (syn/special-form? %))
        def-exps (filter definition? body)]
    (if-not (seq def-exps)
      body
      (let [def-vars (map syn/definition-variable def-exps)
            def-values (map syn/definition-value def-exps)
            body (remove definition? body)]
        (list (syn/make-let
               (syn/make-let-bindings def-vars
                                      (repeat (count def-vars) ''_*unbound*_ ))
               (concat (map syn/make-assignment def-vars def-values) body)))))))

(defn make-procedure [parameters body *env*]
  (list 'procedure parameters (scan-out-defines body) *env*))

(defmethod eval-exp :procedure [exp *env*]
  (make-procedure (syn/lambda-params exp) (syn/lambda-body exp) *env*))

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
          (eval-sequence (procedure-body procedure)
                         (env/extend-with
                            (procedure-parameters procedure)
                            arguments
                            (procedure-environment procedure)))
        :else (throw (Exception. (str "Unknown procedure type - APPLY" procedure)))))

(defmethod eval-exp :application [exp *env*]
  (let [op (eval-exp (syn/operator exp) *env*)
        args (list-of-values (syn/operands exp) *env*)]
    (-apply op args)))

(defn user-format [object]
  (if (compound-procedure? object)
    (list 'compound-procedure
          (procedure-parameters object)
          (procedure-body object)
          '<procedure-env>)
    object))

(defn make-global-environment []
  (env/extend-with (syn/primitive-procedure-names)
                   (syn/primitive-procedure-objects)
                   env/the-empty-environment))

(defn reset-global-environment! []
  (alter-var-root #'the-global-environment
                  (fn [_] (make-global-environment))))

(defn scheme-eval-exp [input]
  (when-not (bound? #'the-global-environment)
    (reset-global-environment!))
  (let [output (eval-exp input the-global-environment)]
        (println (str "\n--> " (user-format output)))
        (newline)))

;; convenience aliases for REPL
(def e scheme-eval-exp)
