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
    ;(display "-----------\n")(display cont)(newline)(display exps)(newline)(display "-----------\n")
    (cond ((null? exps) (display cont)
                        (if (= cont 1) (display " test OK") (display " tests OK"))
                        (newline))
          ((or (eq? (cadr exps) 'ignore) (equal? (car exps) (cadr exps)))
           (iter (cddr exps) (+ cont 1)))
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

; 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

; 3.16

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define x13 (cons 'c nil))
(define x12 (cons 'b x13))
(define x1 (cons 'a x12))

(define x23 (cons 'a nil))
(define x22 (cons x23 nil))
(define x2 (cons x23 x22))

(define x33 (cons 'a nil))
(define x32 (cons x33 x33))
(define x3 (cons x32 x32))

(define x43 (cons 'b nil))
(define x42 (cons 'a x43))
(define x4 (cons x42 nil))
(set-cdr! x4 x4)

(define (test-3-16)
  (test-runner "count-pairs"
               (count-pairs x1) 3
               (count-pairs x2) 4
               (count-pairs x3) 7
               ; (count-pairs x4) -> infinite recursion
               ))

; 3.17

(define (count-pairs x)
  (define visited nil)
  (define (iter x)
    (if (or (not (pair? x)) (contains? x visited))
      0
      (begin
        (set! visited (cons x visited))
        (+ (iter (car x))
           (iter (cdr x))
           1))))
  (iter x))

(define (test-3-17)
  (test-runner "count-pairs"
               (count-pairs x1) 3
               (count-pairs x2) 3
               (count-pairs x3) 3
               (count-pairs x4) 3))

; 3.18

(define (cycle? x)
  (define visited nil)
  (define (iter x)
    (set! visited (cons x visited))
    (cond ((null? (cdr x)) false)
          ((memq (cdr x) visited) true)
          (else (iter (cdr x)))))
  (iter x))

(define (test-3-18)
  (define x1 '(a b c))
  (define x2 '(a b c))
  (set-cdr! x2 x2)
  (let ((x3 (cons x1 x2)))
    (test-runner "cycle?"
                 (cycle? x1) false
                 (cycle? x2) true
                 (cycle? x3) true)))

; 3.19

(define (cycle?-const-space x)
  (define (iter x cont elem num)
    (cond ((null? (cdr x)) false)
          ((eq? x elem) true)
          (else (if (= cont num)
                  (iter (cdr x) 0 x (+ 1 num))
                  (iter (cdr x) (+ cont 1) elem num)))))
  (iter x 0 nil 0))

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x1 '(a b c))
(define x2 '(a b c))
(set-cdr! (last-pair x2) x2)
(define x3 '(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 20))
(define x4 '(a 1 2 3 4 5 6 7 8 9 10 1 2 3 4 15))
(set-cdr! (last-pair x4) x4)
(define x5 (append x3 x4))
(define (test-3-19)
  (test-runner "cycle?-const-space"
               (cycle?-const-space x1) false
               (cycle?-const-space x2) true
               (cycle?-const-space x5) true))

; 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (print-queue q)
  (front-ptr q))

(define (test-3-21)
  (let ((q1 '((a) a))
        (q2 '((a b) b))
        (q3 '((b) b))
        (q4 '(() b)))
    (test-runner "print-queue"
                 (print-queue q1) '(a)
                 (print-queue q2) '(a b)
                 (print-queue q3) '(b)
                 (print-queue q4) '())))

; 3.22

(define (make-queue)
  (let ((front-ptr nil )
        (rear-ptr nil))
    ; definitions of internal procedures
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?) (error "FRONT called on empty queue")
        (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item nil)))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)
                front-ptr))))
    (define (delete!)
      (cond ((empty?) (error "DELETE called on empty queue"))
            (else (set! front-ptr (cdr front-ptr))
                  front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'front) (front))
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) (delete!))
            (else (error 'Unknown operation for queue - ' m))))
    dispatch))

(define (front-queue queue) (queue 'front))
(define (empty-queue? queue) (queue 'empty?))
(define (insert-queue! queue item) ((queue 'insert!) item))
(define (delete-queue! queue) (queue 'delete!))

(define (list-copy l)
  (if (null? l) '()
    (cons (car l) (list-copy (cdr l)))))

(define (test-3-22)
  (let ((q (make-queue)))
    (test-runner "make-queue"
                 (empty-queue? q) true
                 (list-copy (insert-queue! q 1)) '(1)
                 (list-copy (insert-queue! q 2)) '(1 2)
                 (front-queue q) 1
                 (insert-queue! q 3) '(1 2 3)
                 (front-queue q) 1
                 (empty-queue? q) false
                 (delete-queue! q) '(2 3)
                 (front-queue q) 2
                 (delete-queue! q) '(3)
                 (delete-queue! q) '()
                 (empty-queue? q) true)))

; 3.23

(define (make-deque) (cons nil nil))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (set-front! deque item) (set-car! deque item))
(define (set-rear! deque item) (set-cdr! deque item))

(define (get-item deque end)
  (if (empty-deque? deque)
    (error "Trying to retrieve item from empty deque" deque)
    (caar (end deque))))

(define (insert-deque! deque item end)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front! deque new-pair)
           (set-rear! deque new-pair))
          ((eq? end 'front)
           (set-cdr! new-pair (front-ptr deque))
           (set-cdr! (car (front-ptr deque)) new-pair)
           (set-front! deque new-pair))
          (else (set-cdr! (rear-ptr deque) new-pair)
                (set-cdr! (car new-pair) (rear-ptr deque))
                (set-rear! deque new-pair)))))

(define (front-delete-deque deque)
  (cond ((empty-deque? deque) (error "Cannot delete from empty deque" deque))
        (else (set-front! deque (cdr (front-ptr deque)))
              (or (empty-deque? deque) (set-cdr! (car (front-ptr deque)) nil)))))

(define (rear-delete-deque deque)
  (cond ((empty-deque? deque) (error "Cannot delete from empty deque" deque))
        (else (set-rear! deque (cdar (rear-ptr deque)))
              (if (null? (rear-ptr deque)) (set-front! deque nil)
                (set-cdr! (rear-ptr deque) nil)))))

(define (front-insert-deque! deque item) (insert-deque! deque item 'front))
(define (rear-insert-deque! deque item) (insert-deque! deque item 'rear))
(define (front-deque deque) (get-item deque front-ptr))
(define (rear-deque deque) (get-item deque rear-ptr))

(define (print-deque d)
  (define (iter res _d)
    (if (or (null? _d) (empty-deque? _d)) res
      (iter (append res (list (caaar _d))) (cons (cdar _d) (cdr d)))))
  (iter nil d))

(define (test-3-23)
  (let ((x (make-deque)))
    (test-runner "make-deque"
                 (empty-deque? x) #t
                 (front-insert-deque! x 3) 'ignore
                 (front-deque x) 3
                 (front-insert-deque! x 2) 'ignore
                 (front-insert-deque! x 1) 'ignore
                 (front-deque x) 1
                 (empty-deque? x) #f
                 (rear-insert-deque! x 4) 'ignore
                 (front-deque x) 1
                 (rear-deque x) 4
                 (rear-insert-deque! x 5) 'ignore
                 (rear-deque x) 5
                 (print-deque x) '(1 2 3 4 5)
                 (front-insert-deque! x 0) 'ignore
                 (front-deque x) 0
                 (rear-delete-deque x) 'ignore
                 (rear-delete-deque x) 'ignore
                 (rear-deque x) 3
                 (front-deque x) 0
                 (front-delete-deque x) 'ignore
                 (front-delete-deque x) 'ignore
                 (front-delete-deque x) 'ignore
                 (rear-deque x) 3
                 (front-deque x) 3
                 (front-delete-deque x) 'ignore
                 (empty-deque? x) #t
                 (rear-insert-deque! x 1) 'ignore
                 (empty-deque? x) #f
                 (rear-delete-deque x) 'ignore
                 (empty-deque? x) #t
                 )))
