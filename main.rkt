#lang racket

(require "helpers.rkt")
(require "fields.rkt")
(require "drawer.rkt")

; some example density functions:
(def (uniform-sphere-dfunc r (coord origin) (density 1))
  (λ (pos) (if (<= (dist coord pos) r)
               density
               0)))

(def (uniform-sphere-density coord)
  ((uniform-sphere-dfunc 2) coord))

(def (box-density coord)
  (if (<= (apply max coord) 0) 2 0))

; combine them:
(def sphere-plus-box (combined-density-func box-density uniform-sphere-density))

; create space and calculate fields:
(def cspace (get-space sphere-plus-box (coord -4 -4 -4) (coord 5 5 5) 1))


; draw the result
(composite-drawer 50
                  ; show density by the color of the grids:
                  'density (space-slice space-density cspace 'z 0)
                  ; draw vector field:
                  'vectors (space-slice space-field cspace 'z 0))