;;; CHAPTER 4

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

; 4 .25

; In an applcative-order Scheme it (fact 5) would cause an infinite loop in the expression (* n (factorial (- n 1))) with n assuming infinite negative values.
; In a noraml order-language it would work since (* n (factorial (- n 1))) would not be evaluated for n = 1

; 4.26

; unless can easily be derived from if inverting the "true" and "false" expressions

; imagine a situation where we have a list of data points and another list with control values that indicate when the data points are cottupted.
; unless could be used together with map to range over the data points, control values and a list of defalut values and  only take the data ponins over
; the default values when the former are not corrupted
