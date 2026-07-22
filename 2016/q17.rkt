#lang racket

(require file/md5)

(require "utils.rkt")

(day 17)
(testing #f)

;; Part One
(define (valid-hash-pos h d)
  (define c (string-ref h (match d ("U" 0) ("D" 1) ("L" 2) ("R" 3))))
  (or (equal? c #\b) (equal? c #\c) (equal? c #\d) (equal? c #\e) (equal? c #\f)))

(define (get-valid-next pos seed path)
  (define h (bytes->string/utf-8 (md5 (string-append seed path))))
  (match-let (((point x y) pos))
    (filter (λ (p) (match-let (((cons d (point x y)) p))
                     (and (>= x 0) (>= y 0)
                          (<= x 3) (<= y 3)
                          (valid-hash-pos h d))))
            (list (cons "U" (point x (sub1 y)))
                  (cons "D" (point x (add1 y)))
                  (cons "L" (point (sub1 x) y))
                  (cons "R" (point (add1 x) y))))))

(struct fringe-node (pos path) #:transparent)
(define (bfs seed (fringe (list (fringe-node (point 0 0) ""))))
  (match fringe
    ('() "")
    ((cons (fringe-node pos path) fringe) #:when (equal? pos (point 3 3)) path)
    ((cons (fringe-node pos path) fringe)
     (bfs seed (append fringe (for/list ([next (get-valid-next pos seed path)])
                                (match-let (((cons dir pos) next))
                                  (fringe-node pos (string-append path dir)))))))))

(printf "Part one: ~a\n" (bfs (first lines)))

;; Part Two
(define (bfs-possible seed (fringe (list (fringe-node (point 0 0) ""))))
  (match fringe
    ('() '())
    ((cons (fringe-node pos path) fringe) #:when (equal? pos (point 3 3))
                                          (cons path (bfs-possible seed fringe)))
    ((cons (fringe-node pos path) fringe)
     (bfs-possible seed (append fringe (for/list ([next (get-valid-next pos seed path)])
                                         (match-let (((cons dir pos) next))
                                           (fringe-node pos (string-append path dir)))))))))

(printf "Part two: ~a\n" (string-length (last (bfs-possible (first lines)))))
