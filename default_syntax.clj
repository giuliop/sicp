(ns default-syntax)

(defn self-evaluating? [exp]
  (or (number? exp)
      (string? exp)
      (= false exp)
      (= true exp)))

(defn variable? [exp]
  (symbol? exp))

(defn a-list? [exp]
  (or (list? exp) (= clojure.lang.Cons (type exp))))

(def special-forms {
                      :quote  {:type :quotation, :arity #{2}}
                      :set!   {:type :assignment, :arity #{3}}
                      :define {:type :definition, :arity :arbitrary}
                      :if     {:type :if, :arity #{3 4}}
                      :lambda {:type :procedure, :arity #{3 4}}
                      :begin  {:type :list-of-actions, :arity :arbitrary}
                      :cond   {:type :cond, :arity :arbitrary}
                      :or     {:type :or, :arity :arbitrary}
                      :and    {:type :and, :arity :arbitrary}
                      :let    {:type :let, :arity #{3 4}}
                      :let*   {:type :let*, :arity #{3}}
                      :while  {:type :while :arity #{3}}
                      })

(def primitives [
                 ;; :+ +
                 ;; :- -
                 ;; :* *
                 ;; :/ /
                 ;; := =
                 ;; :list list
                 ;; :null?  empty?
                 ;; :car first
                 ;; :cdr rest
                 ;; :cons cons
                 ;; :println println
                 ])

(defn primitive-procedure-names []
  (take-nth 2 primitives))

(defn primitive-procedure-objects []
  (map #(list 'primitive %1) (take-nth 2 (drop 1 primitives))))

(defn arity-ok? [wanted actual]
  (cond (= wanted :arbitrary) true
        :else (contains? wanted actual)))

(defn special-form? [exp]
  (if (empty? exp)
    (throw (Exception. (str "Unquoted empty list " exp)))
    (let [operator (first exp)]
      (if (a-list? operator)
        false
        (let [{:keys [type arity] :as found}
              (get special-forms (keyword operator))]
          (if (not found)
            false
            (if (arity-ok? arity (count exp))
              type
              (throw (Exception. (str "Wrong arity for: " exp "\n" "Expected " arity))))))))))

;; Quotations have the form (quote <text-of-quotation>)
(defn text-of-quotation [exp]
  (second exp))

;; Assignments have the form (set! <var> <value>)
(defn assignment-variable [exp]
  (second exp))

(defn assignment-value [exp]
  (last exp))

;; Definitions have the form (define <var> <value>)
;; or the form (define (<var> <parameter-1> ... <parameter-n>) <body>)
;; The latter form (standard procedure definition) is syntactic sugar for
;; (define <var> (lambda (<parameter-1> ... <parameter-n>) <body>))
(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (first (second exp))))

(defn definition-value [exp]
  (if (symbol? (second exp))
    (last exp)
    (let [params (rest (second exp))
          body (drop 2 exp)]
      (make-lambda params body))))

(defn make-lambda [params body]
  {:pre (list? (first body))} ; body is a list of expressions
  (-> body
      (conj params)
      (conj 'lambda)))

(defn lambda-params [exp]
    (second exp))

(defn lambda-body [exp]
    (drop 2 exp))

;; Conditionals begin with if and have a predicate, a consequent, and an
;; (optional) alternative. If the expression has no alternative part,
;; we provide false as the alternative
(defn if-predicate [exp]
  (second exp))

(defn if-consequent [exp]
  (first (drop 2 exp)))

(defn if-alternative [exp]
  (if (= 4 (count exp))
    (last exp)
    'false))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;; Begin packages a sequence of expressions into a single expression
(defn make-begin [seq]
  (conj seq 'begin ))

(defn sequence-actions [seq-exp] (next seq-exp))
(defn last-exp? [seq] (nil? (next seq)))
(defn first-exp [seq] (first seq))
(defn rest-exps [seq] (next seq))

(defn sequence->exp [seq]
  {:pre [(or (empty? seq)) (list? (first seq))]} ; seq is a list of expressions
  (case (count seq)
    0 seq
    1 (first seq)
    (make-begin seq)))

;; A procedure application is any compound expression that is not one of the
;; above expression types. The car of the expression is the operator, and the
;; cdr is the list of operands
(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))
(defn no-operands? [ops] (empty? ops))
(defn first-operand [ops] (first ops))
(defn rest-operands [ops] (rest ops))

;; cond
(defn cond-clauses [exp] (rest exp))
(defn cond-predicate [clause] (first clause))
(defn cond-actions [clause] (next clause))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))


(defn cond-=>-clause? [clause]
  (= '=> (second clause)))

(defn cond-=>-proc [clause]
  (last clause))

(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false ; no else clause
     (let [first (first clauses)
           rest (rest clauses)]
       (cond (cond-else-clause? first)
             (if (empty? rest) (sequence->exp (cond-actions first))
                 (throw (Exception. (str "ELSE clause isn’t last - COND->IF" clauses))))
             (cond-=>-clause? first)
             (make-if (cond-predicate first)
                      (list (cond-=>-proc first) (cond-predicate first))
                      (expand-clauses rest))
             :else (make-if (cond-predicate first)
                            (sequence->exp (cond-actions first))
                            (expand-clauses rest))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

;; (and c1 ... cn) (or c1 .. cn)
(defn and-clauses [exp]
  (next exp))

(defn or-clauses [exp]
  (next exp))

;; let are derived expressions from lambda
;;    (let ((<var1> <exp1>) ... (<varn> <expn>)) <body>)
;; is equivalent to
;;     ((lambda (<var1> ... <varn>) <body>) <exp1> ... <expn>)
(defn named-let? [exp]
  (symbol? (second exp)))

(defn let-name [exp]
  (second exp))

(defn let-bindings [exp]
  (if (named-let? exp)
    (nth exp 2)
    (second exp)))

(defn let-vars [exp]
  (map first (let-bindings exp)))

(defn let-values [exp]
  (map second (let-bindings exp)))

(defn let-body [exp]
  (if (named-let? exp)
    (drop 3 exp)
    (drop 2 exp)))

;; (defn make-lambda-creator [params body]
;;   (make-lambda () (list (make-lambda params body))))

;; (defn make-callable [name exp]
  ;; (clojure.walk/postwalk-replace {name (list name)} exp))

(defn let->combination [exp]
  (let [values (let-values exp)
        vars (let-vars exp)
        body (let-body exp)
        f (make-lambda vars body)]
    (if (named-let? exp)
      (let [name (let-name exp)
            define-exp (list 'define name f)
            call-exp (conj values (list name))]
        (make-lambda () (list define-exp call-exp)))
      (conj values f))))

  ;; Let* is similar to let, except that the bindings of the let* variables are
  ;; performed sequentially from left to right, and each binding is made in an
;; environment in which all of the preceding bindings are visible
(defn make-let [bindings body]
  (list 'let bindings body))

(defn let*->nested-lets [exp]
  (let [reverse-bindings (reverse (second exp))
        body (last exp)]
    (loop [reverse-bindings reverse-bindings body body]
      (let [let-exp (make-let (list (first reverse-bindings)) body)
            new-reverse-bindings (next reverse-bindings)] 
        (if (seq new-reverse-bindings)
          (recur new-reverse-bindings let-exp)
          let-exp)))))

;; while has the following syntax
;; (while cond exps)
;; e.g.: (while (> 0 x) (display x) (set! x (- x 1)))
(defn while-cond [exp]
  (second exp))

(defn while-exp [exp]
  (drop 2 exp))

(defn while->lambda [exp]
  (let [iter-func (make-lambda ()
                               (list (make-if (while-cond exp) 
                                              (sequence->exp (concat (while-exp exp)
                                                                     '((iter))))
                                         nil)))]
    (make-lambda () (list (list 'define 'iter iter-func) '(iter)))))
