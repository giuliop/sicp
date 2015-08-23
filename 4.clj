;;; CHAPTER 4

(ns chapther4)

; 4.1

;;; stubs to test it
(defn no-operands? [exps]
  (not (seq exps)))

(defn first-operand [exps]
  (first exps))

(defn rest-operands [exps]
  (next exps))

(defn my-eval [exp env]
  (eval exp))
;;;

(defn list-of-values-left-eval [exps env]
  (if (no-operands? exps)
    ()
    (let [first-exp (my-eval (first-operand exps) env)]
      (cons first-exp (list-of-values-left-eval (rest-operands exps) env)))))

(defn list-of-values-right-eval [exps env]
  (if (no-operands? exps)
    ()
    (let [tail-exps (list-of-values-right-eval (rest-operands exps) env)]
      (cons (my-eval (first-operand exps) env) tail-exps))))


; test them with print, expect to see 123 and 321 for left and right eval
; respectively
(defn test4-1 []
  (let [list-of-print-values '((print 1) (print 2) (print 3))
        env ()]
    (list-of-values-left-eval list-of-print-values env)
    (println)
    (list-of-values-right-eval list-of-print-values env)))

; 4.2
; a
; Louis' interpreter will think that assignments, definitions, etc. are procedure
; applications and will fail trying to evaluate them

;b
(defn tagged-list? [exp tag]
  (if (list? exp)
    (= tag (first exp))
    false))

(defn louis-application? [exp]
  (tagged-list? exp 'call))

(defn louis-operator [exp]
  (second exp))

(defn louis-operands [exp]
  (drop 2 exp))

; see the various interpreter.clj for all exercises not answered here. They are functioning Scheme interpreters in Clojure that implements what asked by SICP.
; interpreter-main is the initial onw
; interpreter-analyze-then-execute splits analysis and execution as per section 4.1.7

; 4.21
; a
(comment
  ((lambda (n)
           ((lambda (fib)
                    (fib fib 1 0 n))
            (lambda (fb a b count)
                    (if (= 0 count) b
                      (fb fb (+ a b) a (- count 1)))))) 10)
  )

; b
(comment
  (define (f x)
    ((lambda (even? odd?)
             (even? even? odd? x))
     (lambda (ev? od? n)
             (if (= n 0) 'true (od? ev? od? (- n 1))))
     (lambda (ev? od? n)
             (if (= n 0) 'false (ev? ev? od? (- n 1))))))
  )

; 4.24
(defn timing-tests []
  (require 'interpreter.main)
  (require 'interpreter.analyze-then-execute)
  (letfn [(e1 [exp] (interpreter.main/eval-exp
                     exp interpreter.main/the-global-environment))
          (e2 [exp] (interpreter.analyze-then-execute/eval-exp
                     exp interpreter.analyze-then-execute/the-global-environment))
          (compare [exp] (println exp) (print "Main interpreter -> ") (time (e1 exp))
                   (print "Analyze--execute -> ") (time (e2 exp)))]
    (interpreter.main/reset-global-environment!)
    (interpreter.analyze-then-execute/reset-global-environment!)
    (let [def-exps ['(define (range n)
                   (define (loop lst next)
                     (if (< next 0)
                       lst
                       (loop (cons next lst) (- next 1))))
                   (loop '() n))
                '(define (map f xs)
                  (if (null? xs) nil
                      (cons (f (car xs))
                            (map f (cdr xs)))))
                '(define (mapsquare n)
                   (map (lambda (x) (* x x)) (range n)))]
          time-exps ['(mapsquare 500)]]
      (doseq [x def-exps] (e1 x) (e2 x))
      (doseq [x time-exps] (compare x)))))

(comment
  "
  > (timing-tests)

  (mapsquare 800)
  Main interpreter -> Elapsed time: 29.686206 msecs
  Analyze--execute -> Elapsed time: 20.271033 msecs
 "
  )

; 4 .25

(comment
  In an applcative-order Scheme it (fact 5) would cause an infinite loop in the expression (* n (factorial (- n 1))) with n assuming infinite negative values.
  In a noraml order-language it would work since (* n (factorial (- n 1))) would not be evaluated for n = 1
  )

; 4.26

(comment
  unless can easily be derived from if inverting the "true" and "false" expressions

  imagine a situation where we have a list of data points and another list with control values that indicate when the data points are cottupted.
  unless could be used together with map to range over the data points, control values and a list of defalut values and  only take the data ponins over
  the default values when the former are not corrupted
  )

; 4.28

(comment
  A function operator might be a thunk as in the following contrived example so we cannot simply use eval for it

  (define (id-func f) f)
  ((id-func (lambda (x) x)) 0)

  )

; 4.29

(comment
  (eval-exp '(define (fib n) (cond ((= n 0) 0)
                                   ((= n 1) 1)
                                   (else (+ (fib (- n 1)) (fib (- n 2))))))
            the-global-environment)
  (memoize-on)
  (println "with memoize -> " (time (actual-value '(fib 25) the-global-environment)))
  (memoize-off)
  (println "without -> " (time (actual-value '(fib 25) the-global-environment)))
  (memoize-on)
  
  "With memoize -> Elapsed time:  4896.706411 msecs"
  "Without      -> Elapsed time: 26364.278225 msecs"
  )

(defn ev [exp]
  (interpreter.lazy/eval-exp exp interpreter.lazy/the-global-environment))
(defn act [exp]
  (interpreter.lazy/actual-value exp interpreter.lazy/the-global-environment))
(defn evpr [exp]
  (println (str "\n" (act exp))))
(defn reset-all []
  (interpreter.lazy/memoize-on)
  (interpreter.lazy/reset-global-environment!))

(defn sicp-4-29 []
  (reset-all)
  (ev '(define count 0))
  (ev '(define (id x)
               (set! count (+ count 1))
               x))
  (ev '(define (square x)
               (* x x)))
  
  (interpreter.lazy/memoize-on)
  (evpr '(square (id 10)))
  ;; > 100
  (evpr 'count)
  ;; > 1

  (interpreter.lazy/memoize-off)
  (ev '(define count 0))
  (evpr '(square (id 10)))
  ;; > 100
  (evpr 'count)
  ;; > 2
  )

; 4.30

(defn sicp-4-30 []
  (reset-all)
  (ev '(define (for-each proc items)
          (if (null? items)
            'done
             (begin (proc (car items))
                    (for-each proc (cdr items))))))
  
  (evpr '(for-each (lambda (x) (newline) (display x))
                 (list 57 321 88)))
  
  ;; (ev '(define (for-each-will-not-work proc proc2 items)
          ;; (if (null? items)
            ;; 'done
             ;; (begin (proc proc2 (car items))
                    ;; (for-each-will-not-work proc proc2 (cdr items))))))
  
  ;; (evpr '(for-each-will-not-work (lambda (proc item) (newline) (proc item)(newline))
                                 ;; (lambda (x) (display x))
                                 ;; (list 57 321 88)))
  )

(comment
   display is a primitive procedures so side effects works ok)

(defn sicp-4-30-b []
  (reset-all)
  (ev '(define (p1 x)
         (set! x (cons x '(2)))
         x))
  (ev '(define (p2 x)
         (define (p e)
           e
           x)
         (p (set! x (cons x '(2))))))
  (evpr '(p1 1))
  ; (1 2)
  (evpr '(p2 1))
  ; 1
  (defn Cy-eval-sequence [actions *env*]
    (if (interpreter.default-syntax/last-action? actions)
      (interpreter.lazy/eval-exp
       (interpreter.default-syntax/first-action actions) *env*)
      (do 
        (interpreter.lazy/actual-value
         (interpreter.default-syntax/first-action actions) *env*)
        (recur (interpreter.default-syntax/rest-actions actions) *env*))))

  (alter-var-root (var interpreter.lazy/eval-sequence) (fn [x] Cy-eval-sequence))
  (evpr '(p2 1))
  ; (1 2)
  (use 'interpreter.lazy :reload)
  )

; I like the text approach since it is actually lazy
