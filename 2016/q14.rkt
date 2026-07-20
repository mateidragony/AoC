#lang racket

(require file/md5)
(require "utils.rkt")

(day 14)
(testing #f)

;; Part One
(define (key-triples key)
  (match (regexp-match #px"(.)\\1\\1" key)
    ((list _ n) n)
    (else #f)))

(define (key-quintuples? key c)
  (match (regexp-match (pregexp (format "(~a)\\1\\1\\1\\1" c)) key)
    ((list _ n) #t)
    (else #f)))


(define (dict-remove* d ks)
  (match ks
    ('() d)
    ((cons k ks) (dict-remove* (dict-remove d k) ks))))

(define (get-keys salt idx num-valid total-valid md5 (potential '()))
  (define idx-hash (md5 (string-append salt (number->string idx))))
  (define triples (key-triples idx-hash))
  (define valid-keys (sort (filter (λ (p) (key-quintuples? idx-hash (dict-ref potential p)))
                                   (sort (dict-keys potential) <)) <))
  (if (>= (+ num-valid (length valid-keys)) total-valid)
      (list-ref valid-keys (- total-valid num-valid))
      (get-keys salt (add1 idx) (+ num-valid (length valid-keys)) total-valid md5
                (dict-remove* (dict-remove (if triples (dict-set potential idx triples) potential)
                                           (- idx 1000))
                              valid-keys))))

(printf "Part one: ~a\n" (get-keys (first lines) 0 0 64 md5))

;; Part Two
(define (md5-n s n)
  (if (zero? n) s (md5-n (md5 s) (sub1 n))))

(printf "Part two: ~a\n" (get-keys (first lines) 0 0 64 (λ (s) (md5-n s 2017))))
