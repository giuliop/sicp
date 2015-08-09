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
(def list-of-print-values '((print 1) (print 2) (print 3)))
(def env ())
(defn test4-1 []
  (list-of-values-left-eval list-of-print-values env)
  (println)
  (list-of-values-right-eval list-of-print-values env))

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

; see interpreter.clj for all exercises not answered here. It is a functioning Scheme interprester in Clojure that implements what asked by SICP

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
