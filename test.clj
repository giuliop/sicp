(ns test
  (:require [interpreter :refer (eval-exp the-global-environment
                                          reset-global-environment!)]
            [clojure.test :refer :all]))

(defn ev [exp]
  (eval-exp exp the-global-environment))

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

(deftest let-forms
  (testing "normal let"
    (is (= 10 (ev '(let ((x 5) (y 15)) (- y x))))))
  (testing "let star"
    (is (= 10 (ev '(let* ((x 5) (y (+ 10 x))) (- y x))))))
  (testing "named-let"
    (ev named-let-fib)
    (is (= 8 (ev '(fib 6))))))

(deftest while-form
  (is (= 15 (ev '(let ((x 5) (y 10))
                   (while (> x 0) (set! y (+ y 1)) (set! x (- x 1)))
                   y)))))
