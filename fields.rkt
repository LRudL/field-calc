#lang racket

(require "helpers.rkt")

(provide (all-defined-out))

(def default-space-precision 1)
(def G 1)

(def (coord . args) args)
(def (x-of coord) (car coord))
(def (y-of coord) (cadr coord))
(def (z-of coord) (caddr coord))
(def (dimension coord) (length coord))
(def (coord- c1 c2) (-v c1 c2))
(def (dist^2 coord1 coord2)
  (let ((deltas (-v coord1 coord2)))
    (list-sum (*v deltas deltas))))
(def (dist coord1 coord2) (sqrt (dist^2 coord1 coord2)))

(def origin (coord 0 0 0))

(def (combined-density-func . dfuncs)
  (λ (pos) (list-sum (map (λ (dfunc) (dfunc pos)) dfuncs))))

(def (loop:corner->corner f min-corner max-corner
                          (precision default-space-precision))
  (def (range-of cfunc)
    (range (cfunc min-corner) (cfunc max-corner) precision))
  (def (length-of cfunc)
    (/ (- (cfunc max-corner) (cfunc min-corner)) precision))
  (multiloop ((range-of x-of) (range-of y-of) (range-of z-of))
             ((length-of x-of) (length-of y-of) (length-of z-of))
             (λ (x y z) (f (coord x y z)))))

(def (map-over-space-coords f space)
  (loop:corner->corner f (space-min-corner space) (space-max-corner space)
                       (space-precision space)))

(def (density-space-from-coords dfunc min-corner max-corner
                                (precision default-space-precision))
  (loop:corner->corner dfunc
                       min-corner max-corner
                       precision))

(struct space
  (density min-corner max-corner precision (field #:mutable)))

(def (coord-ranges space)
  (def (ranges minc maxc acc)
    (if (null? minc)
        (reverse acc)
        (ranges (cdr minc) (cdr maxc)
                (cons (range (car minc) (car maxc) acc)))))
  (ranges (space-min-corner space) (space-max-corner space) '()))

(def (make-space dfunc min-corner max-corner
                 (precision default-space-precision))
  (space (density-space-from-coords dfunc min-corner max-corner precision)
         min-corner max-corner precision 'uncalculated))

(def (siref prop-func space ix iy iz) ; Space Index REFerence
  (vector-ref (vector-ref (vector-ref (prop-func space) ix) iy) iz))

(def (space-pos->i space pos)
  (map int (/v (coord- pos (space-min-corner space))
               (space-precision space))))

(def (i-pos->i var val space)
  (let ((i (var->i var)))
    (vector-ref (list->vector (space-pos->i space
                                            (empty-list-with-nth i val 3 0))) i)))

(def (spref prop-func space pos) ; Space Position REFerence
  (apply siref prop-func space (space-pos->i space pos)))

(def (space-slicer prop-func space vars)
  (let* ((ranges
          (filter list?
                  (fill-empty-with-defaults
                   vars
                   (map range (nested-vector-lengths (prop-func space))))))
         (lens (map length ranges))
         (ssiref ((curry siref) prop-func space)))
    (multiloop ((first ranges) (second ranges))
               ((first lens) (second lens))
               (λ (v1 v2)
                 (apply ssiref (fill-from (list v1 v2) vars))))))

(def (var->i var) (cond ((equal? var 'x) 0) ((equal? var 'y) 1) ((equal? var 'z) 2)
                        (#t (raise 'invalid-var))))
(def (space-slice prop-func space var val)
  (let ((sliced (space-slicer prop-func space
                (empty-list-with-nth (var->i var) (i-pos->i var val space) 3))))
    (if (equal? prop-func space-field)
        (nested-vector-map (λ (vec) (drop-nth (var->i var) vec))
                           sliced)
        sliced)))

(def (force-from space target pos)
  (if (= 0 (dist^2 pos target))
      0
      (let ((r (dist pos target))
            (deltas (coord- pos target)))
        (*v (/ (* G (spref space-density space pos)) (dist^2 pos target))
            (list (/ (x-of deltas) r)
                  (/ (y-of deltas) r)
                  (/ (z-of deltas) r))))))

(def (map-over-space prop-func f space)
      (nested-vector-map f (prop-func space)))

(def (fold-over-space prop-func f init space)
      (nested-vector-fold f init (prop-func space)))

(def (force-at-pos space pos)
  (nested-vector-fold +v '(0 0 0)
                      (map-over-space space-density
                                      ((curry force-from) space pos)
                                      space)))

(def (force-field space)
  (if (equal? (space-field space) 'uncalculated)
      (set-space-field! space (map-over-space-coords ((curry force-at-pos) space) space))
      'already-done)
  (space-field space))

(def (get-space dfuncs min-corner max-corner precision)
  (let ((s (make-space dfuncs min-corner max-corner precision)))
    (force-field s)
    s))