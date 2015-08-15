(ns interpreter.analyze-then-execute
  (:require [interpreter.default-syntax :as syn]
            [interpreter.environment :as env]))

(defmacro invariant [& exps]
  `{:pre [~@exps]
    :post [~@exps]})

;; this line is to make sure changes to defmulti are re-evaluated in cider
(def analyze-exp nil)

(defn eval-exp [exp *env*]
  ((analyze-exp exp) *env*))

(def the-global-environment)

(defn debug-value-env [value env]
  (println value)
  (env/pprint env)
  (newline))

(defn -dispatch [exp]
  (cond (syn/self-evaluating? exp) :to-self
        (syn/variable? exp) :variable
        (syn/a-list? exp) (if-let [form (syn/special-form? exp)]
                          form
                         :application)
        :else (throw (Exception. (str "-dispatch / Unknown form " exp)))))

(defmulti analyze-exp -dispatch)

(defmethod analyze-exp :to-self [exp]
  (fn [*env*] exp))

(defn lookup-variable-value [var env]
  (let [value (env/var-value var env)
        next (env/enclosing-environment env)]
    (cond (= value '_*undefined*_) (throw (Exception. (str "Undefined var " var)))
          (= value :not-found) (if (not= next env/the-empty-environment)
                                 (recur var @next)
                                 (throw (Exception. (str "Unbound var " var))))
          :else value)))

(defmethod analyze-exp :variable [exp] 
  (fn [*env*] (lookup-variable-value exp @*env*)))

(defmethod analyze-exp :quotation [exp]
  (let [qval (syn/text-of-quotation exp)]
    (fn [*env*] qval)))

(declare user-format)
(defn set-variable-value! [var value *env*]
  (cond (= *env* env/the-empty-environment)
          (throw (Exception. (str "Unbound variable " var)))
        (not= :not-found (env/var-value var @*env*))
          (do (env/modify! var value *env*)
              (str var " : " (user-format value)))
        :else (recur var value (env/enclosing-environment @*env*))))

(defmethod analyze-exp :assignment [exp]
  (let [var (syn/assignment-variable exp)
        value (syn/assignment-value exp)
        value-fn (analyze-exp value)]
    (fn [*env*] (set-variable-value! var (value-fn *env*) *env*))))

(defn define-variable! [var value *env*]
  (env/modify! var value *env*)
  (str var " : " (user-format value)))

(declare user-format)
(defmethod analyze-exp :definition [exp]
  (let [var (syn/definition-variable exp)
        value (syn/definition-value exp)
        value-fn (analyze-exp value)]
    (fn [*env*] (define-variable! var (value-fn *env*) *env*))))

(defn -true? [x]
  (not (= false x)))

(defn -false? [x]
  (= false x))

(defmethod analyze-exp :if [exp]
  (let [pred (analyze-exp (syn/if-predicate exp))
        true-cond (analyze-exp (syn/if-consequent exp))
        false-cond (analyze-exp (syn/if-alternative exp))]
    (fn [*env*] (if (-true? (pred *env*))
                  (true-cond *env*)
                  (false-cond *env*)))))

;; (defmethod analyze-exp :cond [exp]
;;   (eval-exp (syn/cond->if exp) *env*))

;; (defmethod analyze-exp :or [exp]
;;   (loop [exps (syn/or-clauses exp)
;;          *env* *env*]
;;     (if (seq exps)
;;       (if-let [x (eval-exp (first exps) *env*)]
;;         x
;;         (recur (next exps) *env*))
;;       false)))

;; (defmethod analyze-exp :and [exp]
;;   (loop [exp (syn/and-clauses exp)
;;          *env* *env*
;;          res true]
;;     (if (seq exp)
;;       (if-let [x (eval-exp (first exp) *env*)]
;;         (recur (next exp) *env* x)
;;         false)
;;       res)))

(defmethod analyze-exp :let [exp]
  (analyze-exp (syn/let->combination exp)))

;; (defmethod analyze-exp :let* [exp env]
;;   (eval-exp (syn/let*->nested-lets exp) env))

;; (defmethod analyze-exp :letrec [exp env]
;;   (eval-exp (syn/letrec->let exp) env))

;; (defmethod analyze-exp :while [exp]
;;   (eval-exp (syn/while->lambda exp) *env*))

;; (defn list-of-values-left-eval-exp [exps *env*]
;;   (loop [res () exps exps]
;;     (if (syn/no-operands? exps)
;;       res
;;       (let [value (eval-exp (syn/first-operand exps) *env*)
;;             res (concat res (list value))
;;             exps (syn/rest-operands exps)]
;;         (recur res exps)))))

;; (defn list-of-values-right-eval-exp [exps *env*]
;;   (if (syn/no-operands? exps)
;;     ()
;;     (let [tail-exps (list-of-values-right-eval-exp (syn/rest-operands exps) *env*)
;;           exp (eval-exp (syn/first-operand exps) *env*)]
;;       (conj tail-exps exp) *env*)))

;; (def list-of-values list-of-values-left-eval-exp)

(defn analyze-sequence [actions]
  (let [build-fn (fn [f1 f2] (fn [*env*] (f1 *env*) (f2 *env*)))
        actions-fn (map analyze-exp actions)]
    (if-not (seq actions-fn)
      (throw (Exception. (str "Empty sequence - ANALYZE SEQUENCE")))
      (loop [first-fn (first actions-fn)
             rest-fn (next actions-fn)]
        (if-not (seq rest-fn)
          first-fn
          (recur (build-fn first-fn (first rest-fn)) (next rest-fn)))))))

(defmethod analyze-exp :list-of-actions [exp]
  (analyze-sequence (syn/sequence-actions exp)))

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
  (list 'procedure parameters body *env*))

(defmethod analyze-exp :procedure [exp]
  (let [params (syn/lambda-params exp)
        body (scan-out-defines (syn/lambda-body exp))
        body-fn (analyze-sequence body)]
    (fn [*env*] (make-procedure params body-fn  *env*))))

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
          ((procedure-body procedure)
                         (env/extend-with
                            (procedure-parameters procedure)
                            arguments
                            (procedure-environment procedure)))
        :else (throw (Exception. (str "Unknown procedure type - APPLY" procedure)))))

(defmethod analyze-exp :application [exp]
  (let [op-fn (analyze-exp (syn/operator exp))
        args-fns (map analyze-exp (syn/operands exp))]
    (fn [*env*] (-apply (op-fn *env*) (map #(% *env*) args-fns)))))

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
