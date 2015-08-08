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
                      :quote :quotation
                      :set! :assignment
                      :define :definition
                      :if :if
                      :lambda :procedure
                      :begin :list-of-actions
                      :cond :cond
                      :or :or
                      :and :and
                      :let :let
                      :let* :let*
                      :while :while
                      })

(def primitives [
                 :+ +
                 :- -
                 :* *
                 :/ /
                 := =
                 :< <
                 :> >
                 :list list
                 :null?  empty?
                 :car first
                 :cdr rest
                 :cons cons
                 :display print
                 :newline newline
                 ])

(defn primitive-procedure-names []
  (take-nth 2 primitives))

(defn primitive-procedure-objects []
  (map #(list 'primitive %1) (take-nth 2 (drop 1 primitives))))

(defn special-form? [exp]
  (if (empty? exp)
    (throw (Exception. (str "Unquoted empty list " exp)))
    (let [operator (first exp)]
      (if (a-list? operator)
        false
        (get special-forms (keyword operator))))))

;; Quotations have the form (quote <text-of-quotation>)
(defn text-of-quotation [exp]
  (second exp))

;; Assignments have the form (set! <var> <value>)
(defn assignment-variable [exp]
  (second exp))

(defn assignment-value [exp]
  (last exp))

(defn make-assignment [var value]
  (list 'set! var value))

;; Definitions have the form (define <var> <value>)
;; or the form (define (<var> <parameter-1> ... <parameter-n>) <body>)
;; The latter form (standard procedure definition) is syntactic sugar for
;; (define <var> (lambda (<parameter-1> ... <parameter-n>) <body>))
(defn make-lambda [params body]
  {:pre (list? (first body))} ; body is a list of expressions
  (-> body
      (conj params)
      (conj 'lambda)))

(defn lambda-params [exp]
    (second exp))

(defn lambda-body [exp]
    (drop 2 exp))

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
(defn last-action? [seq] (nil? (next seq)))
(defn first-action [seq] (first seq))
(defn rest-actions [seq] (next seq))

(defn sequence->exp [seq]
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

(defn let->combination [exp]
  (let [values (let-values exp)
        vars (let-vars exp)
        body (let-body exp)
        f (make-lambda vars body)]
    (if (named-let? exp)
      (let [name (let-name exp)
            define-exp (list 'define name f)
            call-exp (conj values name)]
        (list (make-lambda () (list define-exp call-exp))))
      (conj values f))))

(defn make-let-bindings [vars values]
  (partition-all 2 (interleave vars values)))

;; Let* is similar to let, except that the bindings of the let* variables are
;; performed sequentially from left to right, and each binding is made in an
;; environment in which all of the preceding bindings are visible
(defn make-let [bindings body]
  (-> body
      (conj bindings)
      (conj  'let)))

(defn let*->nested-lets [exp]
  (loop [reverse-bindings (reverse (let-bindings exp))
         body (let-body exp)]
    (let [let-exp (list (make-let (list (first reverse-bindings)) body))
          new-reverse-bindings (next reverse-bindings)] 
      (if (seq new-reverse-bindings)
        (recur new-reverse-bindings let-exp)
        (first let-exp)))))

;; while has the following syntax
;; (while cond exps)
;; e.g.: (while (> 0 x) (display x) (set! x (- x 1)))
(defn while-cond [exp]
  (second exp))

(defn while-exp [exp]
  (drop 2 exp))

(defn while->lambda [exp]
  (let [iter-func
        (make-lambda
         ()
         (list (make-if (while-cond exp) 
                        (sequence->exp (concat (while-exp exp) '((iter))))
                        ''nil)))]
    (list (make-lambda () (list (list 'define 'iter iter-func) '(iter))))))
