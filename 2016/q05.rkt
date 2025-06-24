#lang racket

(require file/md5)
(require "utils.rkt")

(day 5)
(testing #f)

;; Part One
(define (valid-hash? str)
  (regexp-match #rx"^00000+[1-9|a|b|c|d|e|f].*$" str))

(define (get-valid-hash str fn (n 0))
  (if (fn (md5 (string-append str (number->string n))))
      (values n (md5 (string-append str (number->string n)))) (get-valid-hash str fn (add1 n))))

(define (get-password str l (n 0))
  (cond
    ((zero? l) '())
    (else
     (define-values (idx h) (get-valid-hash str valid-hash? n))
     (cons (string-ref (bytes->string/utf-8 h) 5) (get-password str (sub1 l) (add1 idx))))))

(printf "Part one: ~a\n" (list->string (get-password (first lines) 8)))

;; Part Two
(define (all-idxs? h)
  (for/and ([i (range 8)])
    (dict-has-key? h i)))

(define (get-password^ str (h (make-immutable-hash)) (n 0))
  (cond
    ((all-idxs? h) h)
    (else
     (define-values (idx hs) (get-valid-hash str valid-hash? n))
     (define str-idx (- (char->integer (string-ref (bytes->string/utf-8 hs) 5))
                        (char->integer #\0)))
     (get-password^ str (dict-set h str-idx (dict-ref h str-idx (string-ref (bytes->string/utf-8 hs) 6)))
                   (add1 idx)))))

(define (hash->str-8 h)
  (list->string (for/list ([i (range 8)]) (dict-ref h i))))

(printf "Part two: ~a\n" (hash->str-8 (get-password^ (first lines))))
