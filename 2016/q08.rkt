#lang racket

(require "utils.rkt")

(day 8)
(testing #f)

(define WIDTH 50)
(define HEIGHT 6)

;; Part One
(define (init-grid)
  (for/vector ([i (range HEIGHT)])
    (for/vector ([j (range WIDTH)])
      #f)))

(define (print-grid g)
  (for ([v g])
    (for ([i v])
      (printf (if i "#" ".")))
    (printf "\n")))

(define (make-rect! g w h)
  (for* ([i (range h)]
         [j (range w)])
    (2d-set! g i j #t)))

(define (rotate-x! g c dc)
  (define len (vector-length g))
  (define g^ (2d-vec-copy g))
  (for ([i (range len)])
    (2d-set! g i c #f))
  (for ([i (range len)])
    (2d-set! g (modulo (+ i dc) len) c (2d-ref g^ i c))))

(define (rotate-y! g r dr)
  (define len (vector-length (vector-ref g 0)))
  (define g^ (2d-vec-copy g))
  (for ([j (range len)])
    (2d-set! g r j #f))
  (for ([j (range len)])
    (2d-set! g r (modulo (+ j dr) len) (2d-ref g^ r j))))

(define (run-instrs g instrs)
  (match instrs
    ('() g)
    ((cons i instrs)
     (define rect (regexp-match #px"rect (\\d+)x(\\d+)" i))
     (define rx   (regexp-match #px"rotate column x=(\\d+) by (\\d+)" i))
     (define ry   (regexp-match #px"rotate row y=(\\d+) by (\\d+)" i))
     (cond
       (rect (make-rect! g (string->number (second rect)) (string->number (third rect))))
       (rx   (rotate-x!  g (string->number (second rx)) (string->number (third rx))))
       (ry   (rotate-y!  g (string->number (second ry)) (string->number (third ry))))
       (else (error "Unknown instruction ~a" i)))
     (run-instrs g instrs))))

(printf "Part one: ~a\n" (for*/fold ([total 0])
                                    ([v (run-instrs (init-grid) lines)]
                                     [i v])
                           (+ total (b->n i))))

;; Part Two
;;(print-grid (run-instrs (init-grid) lines))
(printf "Part two: ~a\n" "cfleloyfcs")
