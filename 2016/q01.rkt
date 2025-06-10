#lang racket

(require "utils.rkt")

(day 1)
(testing #f)

;; Part One

(define (new-dir d d^)
  (match* (d d^)
    (('n "R") 'e)
    (('s "R") 'w)
    (('e "R") 's)
    (('w "R") 'n)
    (('n "L") 'w)
    (('s "L") 'e)
    (('e "L") 'n)
    (('w "L") 's)))

(define (move-dir d m p)
  (match d
    ('n (struct-copy point p [y (+ (point-y p) m)]))
    ('s (struct-copy point p [y (- (point-y p) m)]))
    ('e (struct-copy point p [x (+ (point-x p) m)]))
    ('w (struct-copy point p [x (- (point-x p) m)]))))

(define (run instrs (pos (point 0 0)) (dir 'n))
  (match instrs
    ('() (+ (abs (point-x pos)) (abs (point-y pos))))
    ((cons i instrs) (match-let ([(list _ d^ m) (regexp-match #px"(L|R)(\\d+)" i)])
                       (define dir^ (new-dir dir d^))
                       (run instrs (move-dir dir^ (string->number m) pos) dir^)))))

(printf "Part one: ~a\n" (run (string-split (first lines) ", ")))

;; Part Two
(define (run^ instrs (pos (point 0 0)) (dir 'n) (memo (set)))
  (match instrs
    ('() (error "No repeat points"))
    ((cons i instrs) (match-let ([(list _ d^ m) (regexp-match #px"(L|R)(\\d+)" i)])
                       (define dir^ (new-dir dir d^))
                       (define-values (pos^ repeat? memo^)
                         (for/fold ([pos^ pos] [repeat? #f] [memo^ memo])
                                   ([_ (range (string->number m))])
                           (values (move-dir dir^ 1 pos^)
                                   (or repeat? (if (set-member? memo^ pos^) pos^ #f))
                                   (set-add memo^ pos^))))
                       (if repeat? (+ (abs (point-x repeat?)) (abs (point-y repeat?)))
                           (run^ instrs pos^ dir^ memo^))))))

(printf "Part two: ~a\n" (run^ (string-split (first lines) ", ")))
