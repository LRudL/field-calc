#lang racket

(require racket/draw)
(require racket/draw/arrow)
(require racket/gui)
(require "helpers.rkt")

(provide composite-drawer)


(def (get-scale nv (size-func (λ (x) x)))
  (let ((tnv (nested-vector-map size-func nv)))
    (cons (nested-vector-fold min +inf.0 tnv)
          (nested-vector-fold max -inf.0 tnv))))
(def scale-min car) (def scale-max cdr)

(def (maxmin mn mx v) (min mx (max mn v)))

(def (position-in-scale scale val (exp 1))
  (maxmin 0 1 (/ (expt (- val (car scale)) exp)
                 (expt (- (cdr scale) (car scale)) exp))))

(def (color-from-scale pos min-cols max-cols)
  (apply make-color
          (map int (+v min-cols (*v (maxmin 0 1 pos)
                                    (-v max-cols min-cols))))))

(def (drawer width height f)
  (let* ((target (make-bitmap width height))
         (dc (new bitmap-dc% (bitmap target))))
    (f dc)
    ; change the last line if you want to e.g. save instead of displaying in console:
    (make-object image-snip% target)))

(def (arrow dc xs ys xlen ylen)
  (send dc set-pen "red" 1 'solid)
  (draw-arrow dc xs ys (+ xs xlen) (+ ys ylen) 0 0
              #:arrow-root-radius 0))

(def (draw-density-grid-on-dc
      grid size (mincol '(0 0 0)) (maxcol '(255 255 255)))
  (let ((scale (get-scale grid)))
    (λ (dc)
      (nested-vector-index-map
       (λ (density i j)
         (send dc set-brush (color-from-scale
                             (position-in-scale scale density) mincol maxcol) 'solid)
         (send dc draw-rectangle
               (* i size) (* j size)
               (* (+ i 1) size) (* (+ j 1) size)))
       grid))))

(def (vsize v) (sqrt (list-sum (*v v v))))

(def (to-unit-vector v)
  (/v v (vsize v)))

(def (draw-vector-field-on-dc vfield size)
  (let ((vsize-scale (get-scale vfield vsize)))
    (λ (dc)
      (nested-vector-index-map
       (λ (vec i j)
         (let* ((grid-size-vec (*v (to-unit-vector vec)
                                   (sqrt (/ (* size size) 2))))
                (i-lengths (*v grid-size-vec
                               (position-in-scale vsize-scale
                                                  (vsize vec)
                                                  0.5))))
           (arrow dc
                  (+ (* size i) (/ size 2))
                  (+ (* size j) (/ size 2))
                  (first i-lengths) (second i-lengths))))
       vfield))))

(def (composite-drawer size . args)
  ; args has alternating type symbols and grids
  (let* ((dimensions (nested-vector-lengths (second args))))
    (drawer (* (first dimensions) size) (* (second dimensions) size)
            (λ (dc)
              (def (type-drawer args)
                (if (null? args) 'done
                    (let ((type (first args))
                          (grid (second args)))
                      (match type
                        ('density
                         ((draw-density-grid-on-dc grid size) dc))
                        ('vectors
                         ((draw-vector-field-on-dc grid size) dc)))
                      (type-drawer (cddr args)))))
              (type-drawer args)))))