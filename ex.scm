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

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (square n) (* n n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes from to)
  (cond ( (> from to) (display " DONE "))
        (else (timed-prime-test from)
              (search-for-primes (+ from 1) to))))

(define (first-3-primes n)
  (define (iter n cont)
    (cond ((= cont 3) (display " DONE "))
          ((prime? n)
             (display n)
             (newline)
             (iter (+ n 1) (+ cont 1)))
          (else (iter (+ n 1) cont))))
  (iter n 0))

(define (complete-fermat-test n)
  (define (iter cont)
    (cond ( (= n cont) (display " DONE "))
          ( (= (expmod cont n n) cont) (iter (+ cont 1)) )
          ( else (display cont) )))
  (iter 2))
 
(define (fast-prime? n times)
      (cond ((= times 0) true)
            ((fermat-test n) (fast-prime? n (- times 1)))
            (else false)))
(define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1))))) 
(define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (remainder (square (expmod base (/ exp 2) m)) m))
            (else
             (remainder (* base (expmod base (- exp 1) m))
                        m))))

(define (cube x) (* x x x))
(define (simpson-integral func a b n)
  (define h (/ (- b a) n))
  (define (y k) (func (+ a (* k h))))
  (define (iter k)
    (cond ( (= k n) (y k) )
          ( (= k 0) (+ (y k) (iter 1)) )
          ( (even? k) (+ (* 2 (y k)) (iter (+ k 1))) )
          ( else (+ (* 4 (y k)) (iter (+ k 1))))))
  ( cond ( (> a b) 0 )
         ( else (* (/ h 3) (iter 0)) )))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum2 term a next b)
  (accumulate2 + 0 term a next b))

(define (inc x) (+ x 1))
(define (identity x) x)

(define (simpson-integral2 func a b n)
  (define h (/ (- b a) n))
  (define (y k)
    ( * 
      (cond ( (or (= k 0) (= k n)) 1 )
            ( else (+ 2 (* (remainder k 2) 2)) ))
      (func (+ a (* k h)))))
  ( cond ( (> a b) 0 )
         ( else (* (/ h 3) (sum y 0 inc n) ))))

(define (product term a next b)
  ( if (> a b)
       1
       (* (term a) (product term (next a) next b))))

(define (product2 term a next b)
  (define (iter n result)
    ( if (> n b)
         result
         (iter (next n) (* result (term n)))))
  (iter a 1))

(define (wallis-pi n)
  (define (wallis-term x)
    ( if (even? x)
         (/ (+ x 2) (+ x 1))
         (/ (+ x 1) (+ x 2))))
  (* 4.0 (product2 wallis-term 1 inc n)))

(define (accumulate combiner null-value term a next b)
  ( if (> a b)
       null-value
       (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter n result)
    ( if (> n b)
         result
         (iter (next n) (combiner result (term n)))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value use? term a next b)
  (define (iter n result)
    ( if (> n b)
         result
         (iter (next n)
               ( if (use? n)
                    (combiner result (term n))
                    result))))
  (iter a null-value))

(define (rel-prime-prod n)
  (define (relative-prime? a)
    (= 1 (gcd a n)))
  (filtered-accumulate * 1 relative-prime? identity 1 inc n))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(newline)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      ( if (close-enough? guess next)
           next
           (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (cont-frac n d k)
  (define (iter cont result)
    ( if (= cont 0)
         result
         (iter (- cont 1) (/ (n cont) (+ (d cont) result)))))
  (iter k 0))

(define (cont-frac2 n d k)
  (define (iter cont)
    ( if (= cont k)
         0
         (/ (n cont) (+ (d cont) (iter (+ cont 1))))))
  (iter 1))

(define (euler-e k)
  (define (eu-n x) 1.0)
  (define (eu-d n)
    ( if (= (remainder n 3) 2)
         (* 2 (+ 1 (quotient n 3)))
         1))
  (+ 2 (cont-frac2 eu-n eu-d k)))

(define (tan-cf x k)
  (define (n-tan i)
    ( if (= 1 i)
         x
         (- (square x))))
  (define (d-tan i)
    (- (* 2 i) 1.0))
  (cont-frac n-tan d-tan k))

(define dx 0.00001)
(define (deriv g)
     (lambda (x)
       (/ (- (g (+ x dx)) (g x))
          dx)))
(define (newton-transform g)
     (lambda (x)
       (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
      (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a x x) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (define (iter func cont)
    ( if (= cont n)
         func
         (iter (compose f func) (+ cont 1))))
  (iter f 1))

(define (smooth f)
  (lambda (x) (/ (+ (f(- x dx)) (f(+ x dx)) f(x)) 3))) 

(define (smooth-n f n)
  ((repeat smooth n) f))

(define (iterative-improve good? improve)
  (define (iter x)
    ( if (good? x)
         x
         (iter (improve x))))
  iter)

(define (sqrt-iter x)
  (let ((first-guess 1.0)
        (tolerance 0.0001))
    (define (good? guess)
      (< (abs (- (* guess guess) x)) tolerance))
    (define (improve guess)
      (average guess (/ x guess)))
    ((iterative-improve good? improve) first-guess)))

(define (make-rat n d)
     (let ((g ((if (< d 0)
                -
                +) (gcd n d))))
       (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
     (newline)
     (display (numer x))
     (display "/")
     (display (denom x)))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2.0)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2.0)))

(define (print-point p)
         (newline)
         (display "(")
         (display (x-point p))
         (display ",")
         (display (y-point p))
         (display ")"))

(define (perimeter-rect rect)
  (* 2 (+ (lenght-rect rect) (width-rect rect))))

(define (area-rect rect)
  (* (lenght-rect rect) (width-rect rect)))

(define (lenght-rect rect)
  (car (cdr rect)))

(define (width-rect rect)
  (cdr (cdr rect)))

(define (make-rect p1 height width)
  (cons p1 (cons height width)))

(define (g-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (g-car n)
  (g-list-iter n 2 0))
(define (g-cdr n)
  (g-list-iter n 3 0))
(define (g-list-iter num exp cont)
  ( if (= 0 (modulo num exp))
       (g-list-iter (/ num exp) exp (+ 1 cont))
       cont))

(define (last-pair l)
  (list (list-ref l (- (length l) 1))))

(define (last-pair2 l)
  (let ((tail (cdr l)))
    ( if (null? tail)
         l
         (last-pair2 tail))))

(define (g-reverse l)
  (define (iter l result)
    ( if (null? l)
         result
         (iter (cdr l) (cons (car l) result ))))
  (iter l (list)))

(define (deep-reverse l)
  (define (iter l result)
    ( cond ((null? l) result)
           ( else (iter (cdr l)
                        (cons
                          ( if (pair? (car l))
                               (iter (car l) (list))
                               (car l))
                          result)))))
  (iter l (list)))

(define (fringe l)
  (define (iter l result)
    ( cond ((null? l) result)
           ( else (iter (cdr l) (append result ( if (pair? (car l))
                                                    (iter (car l) nil)
                                                    (list (car l))))))))
  (iter l nil))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

(define (same-parity a . b)
  (define (iter l)
    ( cond ((null? l) (list))
           ((= (modulo a 2) (modulo (car l) 2)) (cons (car l) (iter (cdr l))))
           (else (iter (cdr l)))))
  ( if (null? b)
       (list a)
       (cons a (iter b))))

(define (g-for-each func alist)
  ( cond ((null? alist) (newline))
         (else (func (car alist))
               (g-for-each func (cdr alist)))))

(define X exit)

(define (make-mobile left right)
  (list left right))
(define (make-branch alength structure)
  (list alength structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
(define (total-weight mobile)
  (+ (weight-branch (left-branch mobile)) (weight-branch (right-branch mobile))))
(define (weight-branch branch)
  ( if (pair? (branch-structure branch))
       (total-weight (branch-structure branch))
       (branch-structure branch)))
(define (balanced? mobile)
  (define (torque branch)
    (* (weight-branch branch) (branch-length branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
        (and (= (torque left) (torque right))
             ( if (pair? (branch-structure left))
                  (balanced? (branch-structure left))
                  true)
             ( if (pair? (branch-structure right))
                  (balanced? (branch-structure right))
                  true))))

(define (square-tree tree)
  (cond ((null? tree) nil)
         ((not (pair? tree)) (* tree tree))
         (else (cons (square-tree (car tree))
                     (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         ( if (pair? sub-tree)
              (square-tree2 sub-tree)
              (* sub-tree sub-tree)))
       tree))

(define (tree-map func tree)
  (map (lambda (sub-tree)
         ( if (pair? sub-tree)
              (tree-map func sub-tree)
              (func sub-tree)))
       tree))

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (sub-list)
                         (append (list (car s)) sub-list)) 
                        rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (s-map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))

(define (s-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (s-length sequence)
  (accumulate (lambda (x y)
                     (+ 1 y))
                 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (cond ((pair? x) (count-leaves x))
                           (else 1)))
                   t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) (map (lambda (x) (dot-product x v)) m))

(define (transpose mat) (accumulate-n
                          cons
                          nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (list (dot-product (car cols) x)
                           (dot-product (cadr cols) x)))
         m)))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (enumerate-interval from to)
  (cond ((> from to) nil)
        (else (cons from (enumerate-interval (+ from 1) to)))))

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (nest-pair n)
  (flatmap 
    (lambda (i)
       (map (lambda (j) (list j i))
            (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
        (prime? (+ (car pair) (cadr pair))))

(define (filter predicate seq)
  ( cond ((null? seq) nil)
         ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
         (else (filter predicate (cdr seq)))))

(define (prime-sum-pairs n)
  (filter prime-sum?  (nest-pair n)))

(define (nest-triplet n)
  (flatmap
    (lambda (pair)
      (map (lambda (j) (append pair (list j)))
           (enumerate-interval (+ 1 (max-seq pair)) n)))
    (nest-pair (- n 1))))

(define (max-seq seq)
  ( cond ((null? (cdr (cdr seq)))
          (max (car seq) (cadr seq)))
         (else (max (car seq) (max-seq (cdr seq))))))

(define (sum-triplet sum n)
  (filter (lambda (seq) (= sum (accumulate + 0 seq)))
          (nest-triplet n)))

(define (nth n seq)
  ( if (= n 1)
       (car seq)
       (nth (- n 1) (cdr seq))))

(define (queens board-size)
  (define (queen-cols k)
    ( if (= k 0)
         (list empty-board)
         (filter
           (lambda (positions) (safe? k positions))
           (flatmap
             (lambda (rest-of-queens)
               (map (lambda (new-row)
                      (adjoin-position new-row
                                       k
                                       rest-of-queens))
                    (enumerate-interval 1 board-size)))
             (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define (safe? k positions)
  (let ((new-pos (car positions))
        (others  (cdr positions)))
    (define (safe2? others)
      ( cond ((null? others) true)
             (else (and (no-check? new-pos (car others))
                        (safe2? (cdr others))))))
    (safe2? others)))

(define (no-check? p1 p2)
  (let ((col1 (car p1))
        (col2 (car p2))
        (row1 (cadr p1))
        (row2 (cadr p2)))
    ( cond ((= row1 row2) false)
           ((= col1 col2) false)
           ((= (abs (- row1 row2)) (abs (- col1 col2))) false)
           (else true))))

(define (adjoin-positions new-row k rest-of-queens)
  true)

(define empty-board nil)

(define (up-split painter n)
  ( if (= n 0)
       painter
       (let ((new (up-split painter (- n 1))))
         (below painter (beside new new)))))

(define (below down up)
  (compose-painters (compose-painters up
                                       (build-painter (list "")))
                     down))

(define (beside left right)
  (compose-painters left right))

(define (build-painter symbols)
  (lambda ()
    (define (iter syms)
      ( cond ((null? syms) (display ""))
             ((equal? (car syms) "") (newline)
                                     (iter (cdr syms)))
             (else (display (car syms))
                   (iter (cdr syms)))))
    (iter symbols)))

(define (compose-painters p1 p2)
  (lambda ()
    (p1)
    (p2)))

(define (split first second)
  (lambda (painter n)
    ( if (= n 0)
         painter
         (let ((new ((split first second) painter (- n 1))))
             (first painter (second new new))))))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s vect)
  (make-vect (* s xcor-vect vect) (* s ycor-vect vect)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cadr (cdr frame)))

(define (make-frame-bis origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-bis frame)
  (car frame))

(define (edge1-frame-bis frame)
  (cadr frame))

(define (edge2-frame-bis frame)
  (cdr (cdr frame)))

(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))

(define (start-segment segm)
  (car segm))

(define (end-segment segm)
  (cdr segm))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (outline-painter frame)
  (let ((v1 (make-vect 0 0))
        (v2 (make-vect 0 1))
        (v3 (make-vect 1 1))
        (v4 (make-vect 1 0))
        (s1 (make-segment v1 v2))
        (s2 (make-segment v2 v3))
        (s3 (make-segment v3 v4))
        (s4 (make-segment v4 v1)))
    (segments->painter (list s1 s2 s3 s4))))

(define (split-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (below p1 p2)
  (let ((split-point (make-vect 0.0 0.5))
        (new-p1 (transform-painter p1
                                   (make-vect 0.0 0.0)
                                   (make-vect 1.0 0.0)
                                   split-point))
        (new-p2 (transform-painter p1
                                   split-point
                                   (make-vect 1.0 0.5)
                                   (make-vect 0.0 1.0))))
    (lambda (frame) ((new-p1 frame) (new-p2 frame)))))

(define (below2 p1 p2)
  (rotate-270 (beside (rotate-90 p1) (rotate-90 p2))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (beside (below painter (up-split painter (- n 1)))
            (below (right-split painter (- n 1))
                   (corner-split painter (- n 1))))))

(define (equal2? first second)
  ( cond ((and (pair? first)
               (pair? second))
          (and (equal2? (car first) (car second))
               (equal2? (cdr first) (cdr second))))
         (else (eq? first second))))

;Testing

(define (test-runner name exp1 exp2)
  (display "Testing ")
  (display name)
  (display " : ")
  ( cond ((equal? exp1 exp2) (display "OK"))
         (else (display "FAILED !!!")))
  (newline)
  (display exp1)
  (newline)
  (display exp2)
  (newline))

(let ((l1 '(this is a list))
       (l2 '(this (is a) list)))
      (test-runner "equal2?"
                   (equal2? l1 l1)
                   true)
      (test-runner "equal2?"
                   (equal2? l1 l2)
                   false))

