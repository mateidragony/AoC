#lang racket

(require graph)

(require "utils.rkt")

(day 10)
(testing #f)

;;(for ([line lines]) (printf "~a\n" line))

(define (make-graph g lines)
  (for/fold ([initials (make-immutable-hash)])
            ([line lines])
    (match-let ([val-init (regexp-match #px"value (\\d+) goes to ((bot|output) \\d+)" line)]
                [bot-swap (regexp-match #px"(bot (\\d+)) gives low to ((output|bot) (\\d+)) and high to ((output|bot) (\\d+))" line)])
      (cond
        (val-init (define bot (third val-init))
                  (define val (string->number (second val-init)))
                  (dict-set initials bot (cons val (dict-ref initials bot '()))))
        (bot-swap (define s (second bot-swap))
                  (define dl (fourth bot-swap))
                  (define dh (seventh bot-swap))
                  (add-directed-edge! g s dl 0)
                  (add-directed-edge! g s dh 1)
                  initials)))))

(define g (weighted-graph/directed '()))
(define inputs (make-graph g lines))
(define final-vals
  (for/fold ([bot-vals inputs])
            ([bot (tsort g)])
    (define neighbors (sort (get-neighbors g bot)
                            < #:key (lambda (x) (edge-weight g bot x))))
    (match neighbors
      ('() bot-vals)
      ((list low high) (dict-set (dict-set bot-vals high (cons (apply max (dict-ref bot-vals bot))
                                                               (dict-ref bot-vals high '())))
                                 low (cons (apply min (dict-ref bot-vals bot))
                                           (dict-ref bot-vals low '())))))))

;; Part One
(printf "Part one: ~a\n" (caar (filter (lambda (x) (and (member 17 (cdr x))
                                                        (member 61 (cdr x))))
                                       (dict->list final-vals))))

;; Part Two
(printf "Part two: ~a\n" (* (car (dict-ref final-vals "output 0"))
                            (car (dict-ref final-vals "output 1"))
                            (car (dict-ref final-vals "output 2"))))
