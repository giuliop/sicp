(ns interpreter.lazy
  (:require [interpreter.default-syntax :as syn]
            [interpreter.environment :as env]))

;; this line is to make sure changes to defmulti are re-evaluated in cider
(def eval-exp nil)

(def the-global-environment)

(def memoize-thunk true)
(defn memoize-on []
  (alter-var-root (var memoize-thunk) (fn [x] true)))
(defn memoize-off []
  (alter-var-root (var memoize-thunk) (fn [x] false)))
  

(defn thunk? [exp]
  (and (syn/a-list? exp) (= 'thunk (first exp))))

(defn thunk-exp [exp]
  (second exp))

(defn thunk-env [exp]
  (nth exp 2))

(defn make-evaluated-thunk [value]
  (list 'evaluated-thunk value))

(defn evaluated-thunk? [exp]
  (and (syn/a-list? exp) (= 'evaluated-thunk (first exp))))

(defn thunk-value [evaluated-exp]
  (second evaluated-exp))

(defn actual-value [exp *env*]
  ;; (force-it (eval-exp exp *env*)))
  (let [val (eval-exp exp *env*)]
    (cond (thunk? val)
            (let [result (actual-value (thunk-exp val) (thunk-env val))]
              (when memoize-thunk
                (env/modify! exp (make-evaluated-thunk result) *env*))
              result)
          (evaluated-thunk? val)
            (thunk-value val)
          :else val)))

(defn delay-it [exp *env*]
  (list 'thunk exp *env*))

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
    (cond (= value '_*undefined*_) (throw (Exception. (str "Undefined var " var)))
          (= value :not-found) (if (not= next env/the-empty-environment)
                                 (recur var @next)
                                 (throw (Exception. (str "Unbound var " var))))
          :else value)))

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
  (let [pred (actual-value (syn/if-predicate exp) *env*)]
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

(defmethod eval-exp :letrec [exp env]
  (eval-exp (syn/letrec->let exp) env))

(defmethod eval-exp :while [exp *env*]
  (eval-exp (syn/while->lambda exp) *env*))

(defn list-of-arg-values [exps *env*]
  (loop [res () exps exps]
    (if (syn/no-operands? exps)
      res
      (let [value (actual-value (syn/first-operand exps) *env*)
            res (concat res (list value))
            exps (syn/rest-operands exps)]
        (recur res exps)))))

(defn list-of-delayed-args [exps *env*]
  (loop [res () exps exps]
    (if (syn/no-operands? exps)
      res
      (let [value (delay-it (syn/first-operand exps) *env*)
            res (concat res (list value))
            exps (syn/rest-operands exps)]
        (recur res exps)))))

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
                                      (repeat (count def-vars) ''_*undefined*_ ))
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

(defn -apply [procedure arguments *env*]
  (cond (primitive-procedure? procedure)
          (apply-primitive-procedure procedure (list-of-arg-values arguments *env*))
        (compound-procedure? procedure)
          (eval-sequence (procedure-body procedure)
                         (env/extend-with
                            (procedure-parameters procedure)
                            (list-of-delayed-args arguments *env*)
                            (procedure-environment procedure)))
        :else (throw (Exception. (str "Unknown procedure type - APPLY" procedure)))))

(defmethod eval-exp :application [exp *env*]
  (let [op (actual-value (syn/operator exp) *env*)
        args (syn/operands exp)]
    (-apply op args *env*)))

(defn user-format [object]
  (cond (compound-procedure? object)
          (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>)
        (thunk? object)
          (list 'thunk (thunk-exp object) '<thunk-env>')
        :else object))

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
  (let [output (actual-value input the-global-environment)]
        (println (str "\nlazy --> " (user-format output)))
        (newline)))

;; convenience aliases for REPL
(def e scheme-eval-exp)
