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

(define (test-runner name . exps)
  (define (iter exps cont)
    (cond ((null? exps) (display cont)
                        (if (= cont 1) (display " test OK") (display " tests OK"))
                        (newline))
          ((equal? (car exps) (cadr exps)) (iter (cddr exps) (+ cont 1)))
          (else (display "FAILED test ") (display (+ 1 cont)) (newline)
                (display (car exps)) (newline)
                (display (cadr exps)) (newline))))
  (display "Testing ")
  (display name)
  (display " : ")
  (iter exps 0))


; other funcs

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

;;;     CHAPTHER 3

; 3.1

(define (make-accumulator n)
  (lambda (x) (begin (set! n (+ n x)) n)))

(define (test-1)
  (let ((a (make-accumulator 5)))
    (test-runner "make-accumulator"
                 (a 10) 15
                 (a 10) 25)))
; 3.2

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
    (cond ((eq? x 'how-many-calls) count)
          ((eq? x 'reset-count) (begin (set! count 0) 0))
          (else (begin (set! count (+ count 1)) (f x)))))))

(define (test-2)
  (let ((s (make-monitored sqrt)))
    (test-runner "make-monitored"
                 (s 100) 10
                 (s 'how-many-calls) 1
                 (s 9) 3
                 (s 'how-many-calls) 2
                 (s 'reset-count) 0
                 (s 'how-many-calls) 0)))

; 3.3-3.4

(define (make-account balance password)
  (define wrong-tries 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch user-password m)
    (if (not (eq? user-password password))
      (begin (set! wrong-tries (+ wrong-tries 1))
               (if (< wrong-tries 7) "Incorrect password"
                 "Cops called!"))
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request - MAKE-ACCOUNT"
                         m)))))
  dispatch)

(define (test-3-4)
  (let ((acc (make-account 100 'secret-password)))
    (test-runner "make-account"
                 ((acc 'secret-password 'withdraw) 40) 60
                 (acc 'wrong-password 'deposit) "Incorrect password"
                 (acc 'wrong-password 'deposit) "Incorrect password"
                 (acc 'wrong-password 'deposit) "Incorrect password"
                 (acc 'wrong-password 'deposit) "Incorrect password"
                 (acc 'wrong-password 'deposit) "Incorrect password"
                 (acc 'wrong-password 'deposit) "Incorrect password"
                 (acc 'wrong-password 'deposit) "Cops called!")))

; 3.5

(define (estimate-pi)
  (estimate-integral theP 2 -2 2 -2 10000000))

(define (theP)
  (let ((x (rand-interval-float -2 2))
        (y (rand-interval-float -2 2)))
    (<= (sqrt (+ (* x x) (* y y))) 1)))

(define (is-inside? x low high)
  (and (>= x low) (<= x high)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (area-rect x1 x2 y1 y2) (monte-carlo trials P)))

(define (area-rect x1 x2 y1 y2)
  (* (abs (- x2 x1)) (abs (- y2 y1))))

(define (rand-interval-float low high)
  (let ((r (random-real)))
    (+ low (* r (- high low)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define pi 3.14159265358979323846264338328)

(define (test-3-5)
  (test-runner "estimate-pi" (< (abs (- pi (estimate-pi))) 0.01)
                              true))

; 3.6

(define random-init 5)

(define rand
  (let ((x random-init))
    (lambda (cmd)
      (cond ((eq? cmd 'reset) (lambda (n) (set! x n)))
            ((eq? cmd 'generate)
             (begin (set! x (rand-update x))
                    x))
            (else (error "unknown cmd : " cmd))))))

; simplest stub for rand-update
(define (rand-update x)
  (+ 1 x))

(define (test-3-6)
  (test-runner "rand"
               (rand 'generate)
               (begin (rand 'generate) (rand 'generate) (rand 'generate)
                      ((rand 'reset) random-init) (rand 'generate))))

; 3.7

(define (make-joint acc-name password new-password)
  ((acc-name password 'add-joint) new-password))

(define (make-account balance . passwords)
  (define wrong-tries 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (add-joint new-password)
    (set! passwords (append passwords (list new-password)))
    dispatch)
  (define (dispatch user-password m)
    (if (not (contains? user-password passwords))
      (begin (set! wrong-tries (+ wrong-tries 1))
               (if (< wrong-tries 7) "Incorrect password"
                 "Cops called!"))
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'add-joint) add-joint)
            (else (error "Unknown request - MAKE-ACCOUNT"
                         m)))))
  dispatch)

(define (contains? elem container)
  (cond ((null? container) false)
        ((eq? elem (car container)) true)
        (else (contains? elem (cdr container)))))

(define (test-3-7)
  (let ((peter-acc (make-account 100 'peterpass)))
    (let ((paul-acc (make-joint peter-acc 'peterpass 'paulpass)))
      (test-runner "make-joint"
                 ((paul-acc 'paulpass 'withdraw) 20)
                 80
                 ((peter-acc 'peterpass 'withdraw) 10)
                 70))))

; 3.8

(define f
  (let ((zero false))
    (lambda (x)
      (let ((res (if zero 0 x)))
        (cond ((= x 0) (begin (set! zero true) res))
              (else (begin (set! zero false) res)))))))

(define (test-3-8)
  (test-runner "f"
               (let ((a1 (f 0))
                     (a2 (f 1)))
                 (+ a1 a2))
               0
               (let ((a1 (f 1))
                     (a2 (f 0)))
                 (+ a1 a2))
               1))
