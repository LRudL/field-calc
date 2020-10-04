#lang racket

(provide def) (provide multiloop)
(provide vectorise)
(provide +v) (provide -v) (provide *v) (provide /v)
(provide list-sum)
(provide int)
(provide fill-empty-with-defaults)
(provide fill-from)
(provide empty-list-with-nth)
(provide nested-vector-lengths)
(provide nested-vector-map)
(provide nested-vector-fold)
(provide nested-vector-index-map)
(provide drop-nth)

(define-syntax-rule (def arg ...) (define arg ...))

(define-syntax multiloop-vars
  (syntax-rules ()
    ((multiloop-vars () () f vars) (apply f (reverse vars)))
    ((multiloop-vars (l1 l2 ...) (len1 len2 ...) f vars)
     (for/vector #:length len1 ((i l1)) ; having a #:length improves efficiency
       (multiloop-vars (l2 ...) (len2 ...) f (cons i vars))))))

(define-syntax multiloop
  (syntax-rules ()
    ((multiloop (l1 ...) (len1 ...) f) (multiloop-vars (l1 ...) (len1 ...) f '()))))

(def (int n) (inexact->exact (floor n)))

(def (combine f l1 l2)
  (if (null? l1)
      '()
      (cons (f (car l1) (car l2))
            (combine f (cdr l1) (cdr l2)))))

(def (listify thing other-thing)
    (if (list? thing)
        thing
        (make-list (length other-thing) thing)))

(def (vop f x y)
  (combine f (listify x y) (listify y x)))

(def (mvop f . args)
  (if (= (length args) 2)
      (vop f (car args) (cadr args))
      (apply mvop (cons f
                        (cons (vop f (car args) (cadr args))
                              (cddr args))))))

(def (vectorise f) ((curry mvop) f))

(def +v (vectorise +))
(def -v (vectorise -))
(def *v (vectorise *))
(def /v (vectorise /))

(def list-sum ((curry foldl) + 0))

(def (fill-empty-with-defaults l defaults)
  (cond ((null? l) '())
        ((equal? (car l) 'empty)
         (cons (car defaults)
               (fill-empty-with-defaults (cdr l) (cdr defaults))))
        (#t (cons (car l)
                  (fill-empty-with-defaults (cdr l) (cdr defaults))))))

(def (fill-from fill-list l)
  (def (filler fl l acc)
    (cond ((null? l) (reverse acc))
          ((equal? (car l) 'empty)
           (filler (cdr fl) (cdr l) (cons (car fl) acc)))
          (#t (filler fl (cdr l) (cons (car l) acc)))))
  (filler fill-list l '()))

(def (empty-list-with-nth n nth len (empty-val 'empty))
  (cond ((= len 0) '())
        ((= n 0) (cons nth (empty-list-with-nth (- n 1) nth (- len 1) empty-val)))
        (#t (cons empty-val (empty-list-with-nth (- n 1) nth (- len 1) empty-val)))))

(def (nested-vector-lengths nv)
  (if (vector? nv)
        (cons (vector-length nv) (nested-vector-lengths (vector-ref nv 0)))
        '()))

(def (nested-vector-map f nv)
  (if (vector? (vector-ref nv 0))
      (vector-map (λ (x) (nested-vector-map f x)) nv)
      (vector-map f nv)))

(def (nested-vector-fold f init nv)
  (if (vector? (vector-ref nv 0))
          (foldl f init
                 (vector->list (vector-map (λ (x)
                                             (nested-vector-fold f init x))
                                           nv)))
          (foldl f init (vector->list nv))))

(def (vector-pair la lb)
  (for/vector ((i (vector-length la)))
    (cons (vector-ref la i)
          (vector-ref lb i))))

(def (vector-range n)
  (list->vector (range n)))

(def test
  (vector (vector 0 1 2) (vector 1 2 3) (vector 3 4 5)))

(def (nested-vector-index-map f nv)
  (def (mapper nv indices)
    (let ((paired (vector-pair nv (vector-range (vector-length nv)))))
      (if (vector? (vector-ref nv 0))
          (vector-map (λ (val.i)
                        (mapper (car val.i) (cons (cdr val.i) indices)))
                      paired)
          (vector-map (λ (val.i)
                        (apply f (cons (car val.i)
                                       (reverse (cons (cdr val.i)
                                                      indices)))))
                      paired))))
  (mapper nv '()))

(def (drop-nth n l)
  (cond ((null? l) '())
        ((= n 0) (cdr l))
        (#t (cons (car l) (drop-nth (- n 1) (cdr l))))))