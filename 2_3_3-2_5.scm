;; Some non-standard names that SICP uses
(define true #t)
(define false #f)
(define nil '())

;; random
(random-source-randomize! default-random-source)
(define (random n)
  (random-integer n))

;; runtime
;; (=user + system time consumed by the Scheme interpreter in microseconds)
(define (runtime)
  (* 1000000 (cpu-time)))

;; SICP streams (SICP section 3.5)

(define-macro cons-stream
  (lambda (head tail)
    `(cons ,head (delay ,tail))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? stream) (null? stream))
(define the-empty-stream '())

;; parallel-execute and friends (SICP section 3.4)

;; (if you want to write multithreaded code, you should most likely
;; use the thread abstraction provided by Gambit-C instead of these
;; simpler abstractions; see the Gambit-C documentation for details)

;; This parallel-execute returns a list of the threads generated
;; (in the same order as the argument thunks).
(define (parallel-execute . args)
  (let ((threads (map make-thread args)))
    (for-each thread-start! threads)
    threads))

;; When called with the result of parallel-execute, kills all the threads
;; that the parallel-execute started
(define (kill-parallel-executors thread-list)
  (for-each thread-terminate! thread-list))

;; an implementation of make-serializer as used in SICP
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
    (mutex-lock! mutex)
    (let ((val (apply p args)))
      (mutex-unlock! mutex)
      val))
      serialized-p)))


;Testing

(define (test-runner name exps)
  (define (iter exps cont)
    (cond ((null? exps) (display cont)
                        (if (= cont 1) (display " test OK") (display " tests OK"))
                        (newline))
          ((equal? (eval (car exps)) (cadr exps)) (iter (cddr exps) (+ cont 1)))
          (else (display "FAILED !!!") (newline)
                (display (car exps)) (newline)
                (display (eval (car exps))) (newline)
                (display (cadr exps)) (newline))))
  (display "Testing ")
  (display name)
  (display " : ")
  (iter exps 0))


;(test-runner "deriv"
             ;(deriv '(* (* x y) (+ x 3)) 'x)
             ;'(+ (* x y) (* y (+ x 3))))


;;;     Section 2.3.3

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        (else (union-set (adjoin-set (car set2) set1) (cdr set2)))))

(define (test-2-59) 
  (test-runner "union-set" '(
               (union-set '(1 2 3) '(1 5))
               (5 1 2 3)
               (union-set '() '())
               ()
               )))

; 2.60

(define (element-of-set2? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set2? x (cdr set)))))

(define (adjoin-set2 x set)
  (cons x set))

(define (intersection-set2 s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1)
               (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(define (union-set2 s1 s2)
  (append s1 s2))

; 2.61

(define (adjoin-set x set)
  (define (iter x left right)
    (cond ((null? right) (append left (list x)))
          ((< x (car right)) (append left (list x) right))
          ((= x (car right)) (append left right))
          (else (iter x (append left (list (car right))) (cdr right)))))
  (iter x '() set))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (test-2-61) 
  (test-runner "adjoin-set" '(
               (adjoin-set 3 '(1 2 3))
               (1 2 3)
               (adjoin-set 3 '(1 2 5))
               (1 2 3 5)
               (adjoin-set 4 '())
               (4)
               )))
