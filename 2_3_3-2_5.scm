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


; other funcs

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

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

; 2.62

(define (merge-ord-lists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((= (car l1) (car l2)) (cons (car l1) (merge-ord-lists (cdr l1) (cdr l2))))
        ((< (car l1) (car l2)) (cons (car l1) (merge-ord-lists (cdr l1) l2)))
        (else (cons (car l2) (merge-ord-lists l1 (cdr l2))))))

(define (test-2-62)
  (test-runner "merge-ord-list" '(
                                  (merge-ord-lists '(1 3 6 8) '(2 3 4 7 11))
                                  (1 2 3 4 6 7 8 11))))

; 2.63

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
     (if (null? tree)
         '()
         (append (tree->list-1 (left-branch tree))
                 (cons (entry tree)
                       (tree->list-1
                         (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))

(define (test-2-63)
  (let ((t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
        (t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
        (t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))))
    (display "1 - t1 ->   ")
    (display (tree->list-1 t1))
    (newline)
    (display "2 - t1 ->   ")
    (display (tree->list-2 t1))
    (newline)
    (display "1 - t2 ->   ")
    (display (tree->list-1 t2))
    (newline)
    (display "2 - t2 ->   ")
    (display (tree->list-2 t2))
    (newline)
    (display "1 - t3 ->   ")
    (display (tree->list-1 t3))
    (newline)
    (display "2 - t3 ->   ")
    (display (tree->list-2 t3))))
    (newline)

; 2.65

(define (intersect-ord-list l1 l2)
  (if (or (null? l1) (null? l2))
    '()
    (let ((x1 (car l1)) (x2 (car l2)))
      (cond ((= x1 x2) (cons x1 (intersect-ord-list (cdr l1) (cdr l2))))
            ((< x1 x2) (intersect-ord-list (cdr l1) l2))
            (else (intersect-ord-list l1 (cdr l2)))))))

(define (union-set-tree t1 t2)
  (list->tree (merge-ord-lists (tree->list-2 t1) (tree->list-2 t2))))

(define (intersection-set-tree t1 t2)
  (list->tree (intersect-ord-list (tree->list-2 t1) (tree->list-2 t2))))

; 2.66

(define (lookup the-key db-tree)
  (if (null? db-tree)
    false
    (let ((elem (entry db-tree)) (left (left-branch db-tree)) (right (right-branch db-tree)))
      (cond ((= the-key (key elem)) elem)
            ((> the-key (key elem)) (lookup the-key left))
            (else (lookup the-key right))))))

; 2.67

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit - CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (decode-2-67)
  (decode sample-message sample-tree))

; 2.68

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (define (iter branch)
    (cond ((leaf? branch) nil)
          ((contains-symbol? sym (symbols (left-branch branch)))
           (cons 0 (iter (left-branch branch))))
          (else (cons 1 (iter (right-branch branch))))))
  (if (not (contains-symbol? sym (symbols tree))) (error "bad symbol - " sym)
    (iter tree)))

(define (contains-symbol? sym sym-list)
  (cond ((null? sym-list) false)
        ((eq? sym (car sym-list)) true)
        (else (contains-symbol? sym (cdr sym-list)))))

(define (test-2-68)
  (test-runner "encode" '(
                          (encode '(A D A B B C A) sample-tree)
                          (0 1 1 0 0 1 0 1 0 1 1 1 0))))

; 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge nodes)
  (cond ((null? nodes) '())
        ((null? (cdr nodes)) (car nodes))
         (else (let ((elem (make-code-tree (car nodes) (cadr nodes))))
                 (successive-merge (adjoin-set elem (cddr nodes)))))))

(define (test-2-69)
  (let ((tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
        (mex '(A D A B B C A))
        (code '(0 1 1 0 0 1 0 1 0 1 1 1 0)))
    (equal? (decode code tree) mex)))

; 2.70

(define (test-2-70)
  (let ((pairs '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
        (mex '(get a job sha na na na na na na na na get a job sha na na na na na 
                   na na na wah yip  yip yip yip yip yip yip yip yip sha boom)))
    (display "Huffman encoding lenght (bits): ")
    (display (length (encode mex (generate-huffman-tree pairs))))
    (newline)
    (display "Fixed-lenght encoding lenght (bits): ")
    (display (* (/ (log (length pairs)) (log 2)) (length mex)))
    (newline)))

; 2.73 b/c

(define (install-deriv-package)

  ;; internal procedures
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (cadr s))
  (define (augend s) (accumulate make-sum 0 (cddr s)))
  (define (=number? exp num) (and (number? exp) (= exp num)))

  (define (deriv-prod exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (accumulate make-product 1 (cddr p)))

  (define (deriv-expt exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp)
                                                     (make-sum (exponent exp) -1))
                                (deriv (base exp) var))))
  (define (make-exponentiation base exponent)
    ( cond ((=number? exponent 0) 1)
           ((=number? exponent 1) base)
           (else (list 'expt base exponent))))
  (define (base x) (cadr x))
  (define (exponent x) (caddr x))

  ;; installation
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv 'expt deriv-expt)
  'done)

; 2.74

(define (hq-get-record employee division)
  ((get 'get-employee-record division) employee))

(define (hq-get-salary record division)
  ((get 'get-salary division) employee))

(define (hq-find-employee-record employee division-list)
  (if (null? division-list)
    false
    (or (hq-get-record employee (car division-list))
        (find-employee-record employee (cdr division-list)))))

; 2.78

(define (type datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum - type-tag" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum - contents" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) contents
    (cons type-tag contents)))

;2.79

(define (install-scheme-number-package)
  ;...
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  ;...
  (define (equ-rat? rat1 rat2) (= (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))))
  ;...
  (put 'equ? '(rational rational) equ-rat?)
  'done)

(define (install-complex-package)
  ;...
  (define (equ-img? i1 i2) (and (= (real-part i1) (real-part i2)) (= (imag-part i1) (imag-part i2))))
  ;...
  (put 'equ? '(complex complex) equ-img?)
  'done)

(define (equ? x y) (apply-generic 'equ? x y))

; 2.82

(define (all-true args)
  (cond ((null? args) false)
        ((null? (cdr args)) (and (car args) true))
        (else (and (car args) (all-true (cdr args))))))

(define (maplist funcs args)
  (let ((f? (null? funcs)) (a? (null? args)))
    (cond ((and f? a?) nil)
          ((or (and f? (not a?)) (and a? (not f?))) (error "different lenght args for maplist"))
          (else (cons ((car funcs) (car args)) (maplist (cdr funcs) (cdr args)))))))

(define (apply-generic op . args)

  (define (iter-on-types types-to-try type-tags)
    (if (null? types-to-try) (error "No method for these types" (list op type-tags))
      (let ((try-type (car types-to-try)))
        (let ((coerce-funcs (map (lambda (x) (if (eq? x try-type)
                                               (lambda (x) x)
                                               (get-coercion x try-type)))
                                 type-tags)))
          (if (all-true coerce-funcs)
            (let ((proc (get op (map (lambda (x) try-type) type-tags))))
              (if proc (apply proc (map contents (maplist coerce-funcs args)))
                (iter-on-types (cdr types-to-try) type-tags)))
            (iter-on-types (cdr types-to-try) type-tags))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (> (length args) 1)
          (iter-on-types type-tags type-tags)
          (error "No method for this type"
                 (list op type-tags)))))))

; creating test stubs
(define (type-tag x) (car x))
(define (contents x) (cdr x))
(define (get-coercion x y)
  (if (and (eq? 'mammal y) (or (eq? x 'dog) (eq? x 'cat) (eq? x 'horse)))
    (lambda (x) (list 'mammal (cadr x)))
    false))
(define (get op types)
  (cond ((and (eq? op mammal-love) (all-true (map (lambda (x) (eq? 'mammal x)) types)))
         mammal-love)
        ((and (eq? op mammal-fish-love)
              (all-true (map (lambda (x) (or (eq? 'mammal x) (eq? 'fish x))) types)))
         mammal-fish-love)
        (else false)))
(define (mammal-love . mammals)
  (define (iter mammals)
    (if (null? mammals) (list "the end !")
      (append (list (caar mammals) "loves") (iter (cdr mammals)))))
  (iter mammals))
(define mammal-fish-love mammal-love)
(define adog '(dog "a dog"))
(define acat '(cat "a cat"))
(define ahorse '(mammal "a horse"))
(define afish '(fish "a fish"))

(define (test-2.82)
  (test-runner "apply-generic"
               '((apply-generic mammal-love adog acat ahorse)
                 ("a dog" "loves" "a cat" "loves" "a horse" "loves" "the end !"))))
; example of failure that should work
(define (failed-test-2.82)
  (apply-generic mammal-fish-love adog acat afish))

; 2.83

(define (install-int-package)
  ;...
  (define (raise-int int)
    (make-rational int 1))
  (put 'raise 'int raise-int)
  'done)

(define (install-rat-package)
  ;...
  (define (raise-rat rat)
    (make-real (/ (numer rat) (denom rat))))
  (put 'raise 'rat raise-rat)
  'done)

(define (install-real-package)
  ;...
  (define (raise-real real)
    (make-complex-from-real-imag real 0))
  (put 'raise 'real raise-real)
  'done)

; 2.84
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (> (length args) 1)
          (let ((raised-args (raise-to-common args)))
            (if raised-args
              (let ((proc (get op (map type-tag raised-args))))
                (if proc
                  (apply proc (map contents raised-args))
                  (error "No method for these types" (list op type-tags))))
              (error "No method for these types" (list op type-tags))))
          (error "No method for this type" (list op type-tags)))))))

(define (raise-to-common args)
    (let ((raised-args (map (lambda (x) (raise-to-type (top-type-of args) x)) args)))
      (if (all-true raised-args) raised-args
        false)))

(define (raise-to-type type item)
  (let ((item-type (type-tag item)))
    (if (eq? item-type type) item
        (let ((raise-fn (get-raise item-type)))
          (if raise-fn (raise-to-type type (raise-fn item))
            false)))))

(define (top-type-of args)
  (if (null? (cdr args)) (type-tag (car args))
    (let ((t1 (type-tag (car args)))
          (t2 (top-type-of (cdr args))))
      (let ((l1 (get-level t1)) (l2 (get-level t2)))
        (if (> l1 l2) t1 t2)))))

; creating test stubs
(define aint '(int 3))
(define arat '(rat 5 8))
(define areal '(real 3.5))
(define acomplex '(complex 10 5))
(define (get-raise type)
  (cond ((eq? type 'int) (lambda (int) (list 'rat (cadr int) 1)))
        ((eq? type 'rat) (lambda (rat) (list 'real (/ (cadr rat) (caddr rat)))))
        ((eq? type 'real) (lambda (real) (list 'complex (cadr real) 0)))
        (else false)))
(define (get op types)
  (if (and (eq? op 'sum-nums) (all-true (map (lambda (x) (eq? x 'complex)) types)))
    sum-complex
    false))
(define (sum-complex . nums)
  ;(nums))
  (define (sum-2-complex i1 i2)
    (list (+ (car i1) (car i2)) (+ (cadr i1) (cadr i2))))
  (append '(complex) (accumulate sum-2-complex '(0 0) nums)))
(define (get-level type)
  (cond ((eq? type 'int) 1)
        ((eq? type 'rat) 2)
        ((eq? type 'real) 3)
        ((eq? type 'complex) 4)
        (else (error "no level for type" type))))

(define (test-2.84)
  (test-runner "apply-generic" '(
    (apply-generic 'sum-nums aint arat areal acomplex)
    (complex 17.125 5))))

; 2.85

; install-complex-package...
(define (project-complex imag)
  (make-real (real-part imag)))
; (put-project 'complex project-complex)

; install-real-package
(define (project-real num)
  (define (find-denom n d)
    (if (integer? (* n d)) d
      (find-denom n (* d 10))))
  (let ((real (car num)))
    (let ((denom (find-denom real 1)))
      (make-rational (inexact->exact (* real denom)) denom))))
; put-project...

; install-rational-package....
(define (project-rat rat)
  (make-int (inexact->exact (round (/ (numer rat) (denom rat))))))
; put-project...

(define (project arg)
  (let ((fn (get-project (type-tag arg))))
    (if fn (fn (contents arg)) false)))

(define (drop arg)
  (let ((projected (project arg)))
    (if (and projected (equ? arg ((get-raise (type-tag projected)) projected))) (drop projected)
      arg)))

(define (new-apply-generic op . args)
  (drop (apply apply-generic op args)))

; test stubs
(define (make-rational n d)
  (let ((div (gcd n d)))
    (list 'rat (/ n div) (/ d div))))
(define (make-real r)
  (list 'real r))
(define (make-int i)
  (list 'int i))
(define (get-project type)
  (cond ((eq? type 'complex) project-complex)
        ((eq? type 'real) project-real)
        ((eq? type 'rat) project-rat)
        (else false)))
(define (equ? x y)
  (let ((tx (type-tag x)) (ty (type-tag y)) (cx (contents x)) (cy (contents y)))
    (cond ((not (eq? tx ty)) false)
          ((eq? tx 'int) (= (car cx) (car cy)))
          ((eq? tx 'rat) (= (* (numer cx) (denom cy)) (* (numer cy) (denom cx))))
          ((eq? tx 'real) (= (car cx) (car cy)))
          ((eq? tx 'complex) (and (= (real-part cx) (real-part cy)) (= (imag-part cx) (imag-part cy))))
          (else (error "unknown type" tx)))))
(define (numer rat) (car rat))
(define (denom rat) (cadr rat))
(define (real-part imag) (car imag))
(define (imag-part imag) (cadr imag))

(define (test-2.85)
  (test-runner "new-apply-generic"
               '((new-apply-generic 'sum-nums (make-int 5) (make-rational 5 2)
                                    (make-real 7.5) (list 'complex 10 0))
                 (int 25)
                 (new-apply-generic 'sum-nums (make-int 5) (make-rational 5 2)
                                    (make-real 8) (list 'complex 10 0))
                 (rat 51 2))))

; i can't force myself to complete the next few exercises of this chapter, it's
; boring to death i'll move to chapter 3, i'm doing this for fun after all, dude!
