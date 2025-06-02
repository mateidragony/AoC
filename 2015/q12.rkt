#lang racket

(require "utils.rkt")
(require json)

(day 12)

;; (for ([line lines]) (printf "~a\n" line))

;; Part One                
(printf "Part one: ~a\n" (sum (map string->number (regexp-match* #px"(-?\\d+)" (first lines)))))

;; Part Two
(define (sum-non-reds json)
  (cond
    ((list? json) (sum (map sum-non-reds json)))
    ((hash? json)
     (define-values (ret _)
       (for/fold ([total 0]
                  [found-red #f])
                 ([(k v) json])
         (cond
           (found-red (values 0 #t))
           ((equal? v "red") (values 0 #t))
           (else (values (+ total (sum-non-reds v)) #f)))))
     ret)
    (else (if (number? json)
              json 0))))

(printf "Part two: ~a\n" (sum-non-reds (string->jsexpr (first lines))))
