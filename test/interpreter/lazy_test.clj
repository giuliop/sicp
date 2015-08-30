(ns interpreter.lazy-test
  (:require [interpreter.lazy :refer (the-global-environment
                                      reset-global-environment!
                                      scan-out-defines
                                      user-format)]
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
    (is (= 3 (act '3)))
    (is (= 0 (act '0)))
    (is (= 7.5 (act '7.5)))
    (is (= 3/2 (act '3/2)))
    (is (= -35.4 (act '-35.4))))
  (testing "strings"
    (is (= "" (act '"")))
    (is (= "hello dude!" (act '"hello dude!"))))
  (testing "booleans"
    (is (= false (act 'false)))
    (is (= false (act ''false)))
    ;; (is (= false (act '#f)))
    (is (= true (act ''true)))
    (is (= true (act 'true)))
    ;; (is (= true (act '#t)))
  ))

(deftest quote
  (is (= 'hello (act '(quote hello))))
  (is (= '''2 (user-format (act '(quote (quote (quote 2)))))))
  )

(deftest assignment
  (is (thrown? Exception (ev '(set! x 10))))
  (ev '(define x 5))
  (is (= 5 (act 'x)))
  (is (= "x : 10" (ev '(set! x 10))))
  )

(deftest conditional
  (testing "if"
    (is (= 0 (act '(if false 1 0))))
    (is (= 1 (act '(if true 1 0)))))
  (testing "cond"
    (is (= 0 (act '(cond (false 1)
                        (false 2)
                        (else 0)))))
    (is (= 1 (act '(cond (true 1)
                        (else 0))))))
  )

(deftest and-or
  (is (= 'ok (act '(and 1 1 'ok))))
  (is (= false (act '(and 1 (= 3 5) 'ok))))
  (is (= true (act '(and))))
  (is (= 'ok (act '(or false 'ok true))))
  (is (= false (act '(or false (= 3 5)))))
  (is (= false (act '(or))))
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
    (is (= 10 (act '(let ((x 5) (y 15)) (- y x))))))
  (testing "let star"
    (is (= 10 (act '(let* ((x 5) (y (+ 10 x))) (- y x))))))
  (testing "letrec"
    (is (= 3628800) (act '(letrec ((fact (lambda (n)
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

(deftest scan-out-defines-from-functions
  (let [in '(lambda (a b)
                    (define x 5)
                    (define y (* 2 a))
                    ('exp1) ('exp2) ('exp3))
        out '((let ((x '_*undefined*_)
                    (y '_*undefined*_))
                 (set! x 5)
                 (set! y (* 2 a))
                 ('exp1) ('exp2) ('exp3)))]
    (is (= out (scan-out-defines (interpreter.default-syntax/lambda-body in)))))
  (let [in '(lambda (x) (newline) x)
        out '((newline) x)]
    (is (= out (scan-out-defines (interpreter.default-syntax/lambda-body in))))))

(deftest lambda
  (ev '(define (map f xs)
         (if (null? xs) '()
             (cons (f (car xs))
                   (map f (cdr xs))))))
  (ev '(define (list-ref items n)
         (if (= n 0)
           (car items)
           (list-ref (cdr items) (- n 1)))))
  (ev '(define x (map (lambda (x) (* x x)) '(2 3 4 5))))
  (is (= 4 (act '(car x))))
  (is (= 9 (act '(list-ref x 1))))
  (is (= 16 (act '(list-ref x 2))))
  (is (= 25 (act '(list-ref x 3))))
  (is (= () (act '(cdr (cdr (cdr (cdr x)))))))
  )

(deftest Y-operator
  "testing even-odd"
  (ev '(define (f x)
         ((lambda (even? odd?)
                  (even? even? odd? x))
          (lambda (ev? od? n)
                  (if (= n 0) 'true (od? ev? od? (- n 1))))
          (lambda (ev? od? n)
                  (if (= n 0) 'false (ev? ev? od? (- n 1)))))))
  (is (= true (act '(f 10))))
  (is (= false (act '(f 1))))
  (is (= true (act '(f 0))))
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

(deftest list-as-streams
  ;; (ev '(define (cons x y)
          ;; (lambda (m) (m x y))))
  ;; (ev '(define (car z)
         ;; (z (lambda (x y) x))))
  ;; (ev '(define (cdr z)
         ;; (z (lambda (x y) y))))
  (ev '(define (add-lists list1 list2)
         (cond ((null? list1) list2)
               ((null? list2) list1)
               (else (cons (+ (car list1) (car list2))
                           (add-lists (cdr list1) (cdr list2)))))))
  (ev '(define (list-ref items n)
         (if (= n 0)
           (car items)
           (list-ref (cdr items) (- n 1)))))
  (ev '(define ones
         (cons 1 ones)))
  (ev '(define integers
         (cons 1 (add-lists ones integers))))
  (is (= 18 (act '(list-ref integers 17))))
  (is (= 'a (act '(car '(a b c)))))
  (is (= 2 (act '(car (cdr '(1 2 3))))))
)

(deftest lazy-list-printing
  (is (= '(1 2 3 4 5 ...) (user-format (act '(cons 1 '(2 3 4 5 6 7 8 9)))))))
