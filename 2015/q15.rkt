#lang racket

(require "utils.rkt")

(day 15)

(struct ingredient (cp d f t cl) #:transparent)

;; Part One
(define (parse-ingerdients lines)
  (for/hash ([line lines])
    (match-let (((list _ n cp d f t cl) (regexp-match #px"([a-zA-Z]+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)" line)))
      (values n (ingredient (string->number cp) (string->number d) (string->number f) (string->number t) (string->number cl))))))

(define ingredients (parse-ingerdients lines))

(define (n-combinations n ls)
  (if (zero? n)
      (list (for/fold
                ([d (make-immutable-hash)]) ([e ls])
              (dict-set d e 0)))
      (match ls
        ('() '())
        ((list v) (list (dict-set (make-immutable-hash) v n)))
        ((cons v ls)
         (for*/list ([i (inclusive-range 0 n)]
                     [d (n-combinations (- n i) ls)])
           (dict-set d v i))))))

(define (score is cs)
  (define ingredient-fs (list ingredient-cp ingredient-d ingredient-f ingredient-t))
  (apply * (for/list ([f ingredient-fs])
             (max 0 (apply + (for/list ([(n i) is])
                               (* (dict-ref cs n) (f i))))))))

(printf "Part one: ~a\n" (apply max (map (curry score ingredients) (n-combinations 100 (dict-keys ingredients)))))

;; Part Two
(define (calories is cs)
  (sum (for/list ([(n i) is])
       (* (dict-ref cs n) (ingredient-cl i)))))

(define (score^ is cs)
  (if (equal? (calories is cs) 500)
      (score is cs)
      0))

(printf "Part two: ~a\n" (apply max (map (curry score^ ingredients) (n-combinations 100 (dict-keys ingredients)))))
