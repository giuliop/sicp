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

