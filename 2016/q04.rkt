#lang racket

(require "utils.rkt")

(day 4)
(testing #f)

(struct room (name sid check) #:transparent)

;; Part One
(define (parse-room str)
  (match-let ([(list _ _ sid check) (regexp-match #px"([a-z]+-)+(\\d+)\\[([a-z]+)\\]" str)])
    (room (drop-right (string-split str "-") 1) (string->number sid) check)))

(define (letter-freqs str)
  (for/fold ([freqs (make-immutable-hash)])
            ([c str])
    (dict-set freqs c (add1 (dict-ref freqs c 0)))))

(define (make-checksum r)
  (define freqs (letter-freqs (string-join (room-name r) "")))
  (list->string (take (sort (sort (dict-keys freqs) char<?) (λ (a b) (> (dict-ref freqs a) (dict-ref freqs b)))) 5)))

(define rooms (map parse-room lines))
(define valid-rooms (filter (λ (r) (string=? (make-checksum r) (room-check r))) rooms))

(printf "Part one: ~a\n" (for/sum ([r valid-rooms])
                           (room-sid r)))

(define (decrypt r)
  (cons (string-join (for/list ([n (room-name r)])
                       (list->string
                        (for/list ([c n])
                          (integer->char
                           (+ (modulo (+ (- (char->integer c) (char->integer #\a))
                                         (room-sid r)) 26)
                              (char->integer #\a)))))))
        (room-sid r)))

;; Part Two
(printf "Part two: ~a\n" (cdr (first (filter (λ (v) (string=? "northpole object storage" (car v)))
                                             (map decrypt valid-rooms)))))
