(ns interpreter.lazy-test
  (:require [interpreter.lazy :refer (the-global-environment
                                      reset-global-environment!)]
            [clojure.test :refer :all]))

(defn ev [exp]
  (interpreter.lazy/eval-exp exp the-global-environment))

(defn act [exp]
  (interpreter.lazy/actual-value exp the-global-environment))

(defn setup [f]
  (reset-global-environment!)
  (f)
(reset-global-environment!))

(use-fixtures :each setup)

(deftest self-evaluating
  (testing "numbers"
    (is (= 3 (ev '3)))
    (is (= 0 (ev '0)))
    (is (= 7.5 (ev '7.5)))
    (is (= 3/2 (ev '3/2)))
    (is (= -35.4 (ev '-35.4))))
  (testing "strings"
    (is (= "" (ev '"")))
    (is (= "hello dude!" (ev '"hello dude!"))))
  (testing "booleans"
    (is (= false (ev 'false)))
    (is (= false (ev ''false)))
    ;; (is (= false (ev '#f)))
    (is (= true (ev ''true)))
    (is (= true (ev 'true)))
    ;; (is (= true (ev '#t)))
  ))

(deftest quote
  (is (= 'hello (ev '(quote hello))))
  (is (= '(quote (quote 2)) (ev '(quote (quote (quote 2))))))
  )

(deftest assignment
  (is (thrown? Exception (ev '(set! x 10))))
  (ev '(define x 5))
  (is (= 5 (ev 'x)))
  (is (= "x : 10" (ev '(set! x 10))))
  )

(deftest conditional
  (testing "if"
    (is (= 0 (ev '(if false 1 0))))
    (is (= 1 (ev '(if true 1 0)))))
  (testing "cond"
    (is (= 0 (ev '(cond (false 1)
                        (false 2)
                        (else 0)))))
    (is (= 1 (ev '(cond (true 1)
                        (else 0))))))
  )

(deftest and-or
  (is (= 'ok (ev '(and 1 1 'ok))))
  (is (= false (ev '(and 1 (= 3 5) 'ok))))
  (is (= true (ev '(and))))
  (is (= 'ok (ev '(or false 'ok true))))
  (is (= false (ev '(or false (= 3 5)))))
  (is (= false (ev '(or))))
  )

(def named-let-fib '(define (fib n)
                      (let fib-iter ((a 1)
                                     (b 0)
                                     (count n))
                           (if (= count 0)
                             b
                             (fib-iter (+ a b) a (- count 1))))))

(deftest letrec->let
  (let [in '(letrec ((a a-value) (b b-value) (c c-value))
                    (exp1) (exp2) (exp3))
        out '(let ((a '_*undefined*_) (b '_*undefined*_) (c '_*undefined*_))
               (set! a a-value)
               (set! b b-value)
               (set! c c-value)
               (exp1) (exp2) (exp3))]
    (is (= out (interpreter.default-syntax/letrec->let in)))))

(deftest let-forms
  (testing "normal let"
    (is (= 10 (ev '(let ((x 5) (y 15)) (- y x))))))
  (testing "let star"
    (is (= 10 (ev '(let* ((x 5) (y (+ 10 x))) (- y x))))))
  (testing "letrec"
    (is (= 3628800) (ev '(letrec ((fact (lambda (n)
                                                (if (= n 1) 1
                                                    (* n (fact (- n 1)))))))
                                 (fact 10)))))
  (testing "named-let"
    (ev named-let-fib)
    (is (= 8 (act '(fib 6))))))

;; (deftest while-form
  ;; (is (= 15 (act '(let ((x 5) (y 10))
                   ;; (while (> x 0) (set! y (+ y 1)) (set! x (- x 1)))
                   ;; y)))))

(deftest scan-out-defines
  (let [in '(lambda (a b)
                    (define x 5)
                    (define y (* 2 a))
                    ('exp1) ('exp2) ('exp3))
        out '((let ((x '_*undefined*_)
                    (y '_*undefined*_))
                 (set! x 5)
                 (set! y (* 2 a))
                 ('exp1) ('exp2) ('exp3)))]
    (is (= out (interpreter.lazy/scan-out-defines (interpreter.default-syntax/lambda-body in)))))
  (let [in '(lambda (x) (newline) x)
        out '((newline) x)]
    (is (= out (interpreter.lazy/scan-out-defines (interpreter.default-syntax/lambda-body in))))))

(deftest lambda
  (ev '(define (map f xs)
         (if (null? xs) nil
             (cons (f (car xs))
                   (map f (cdr xs))))))
  (is (= '(4 9 16 25) (ev '(map (lambda (x) (* x x)) '(2 3 4 5))))))

(deftest Y-operator
  "testing even-odd"
  (ev '(define (f x)
         ((lambda (even? odd?)
                  (even? even? odd? x))
          (lambda (ev? od? n)
                  (if (= n 0) 'true (od? ev? od? (- n 1))))
          (lambda (ev? od? n)
                  (if (= n 0) 'false (ev? ev? od? (- n 1)))))))
  (is (= true (ev '(f 10))))
  (is (= false (ev '(f 1))))
  (is (= true (ev '(f 0))))
  "testing fib"
  (ev '(define Y-fib (lambda (n)
                            ((lambda (fib)
                                     (fib fib 1 0 n))
                             (lambda (fb a b count)
                                     (if (= 0 count) b
                                         (fb fb (+ a b) a (- count 1))))))))
  (is (= 55 (act '(Y-fib 10)))))

(deftest try
  (ev '(define (try a b)
          (if (= a 0) 1 b)))
  (ev '(try 0 (/ 1 0))))

(deftest thunk-memoization
  (ev '(define count 0))
  (ev '(define (id x) (set! count (+ count 1)) x))
  (ev '(define w (id (id 10))))
  (is (= 1 (act 'count)))
  (is (= 10 (act 'w)))
  (is (= 2 (act 'count)))
  )

(deftest optional-lazyness
  (ev '(define count 0))
  (ev '(define f a (b lazy) c (d lazy-memo)
         (id a) (id a)
         (id b) (id b)
         (id c) (id c)
         (id d) (id d)
        'done))
  (ev '(f (set! count (+ count 1))
          (set! count (+ count 1))
          (set! count (+ count 1))
          (set! count (+ count 1))))
  (is (= 5 (act 'count)))
  )
