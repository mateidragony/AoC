#lang racket

(require "utils.rkt")

(day 8)

;; Part One
(define (replace-quote str)
  (regexp-replace* #rx"\\\\\"" str "Q"))
(define (replace-slash str)
  (regexp-replace* #rx"\\\\\\\\" str "S")) ;; zamn
(define (replace-hex str)
  (regexp-replace* #px"\\\\x(\\d|a|b|c|d|e|f){,2}" str "H"))
(define replace-escapees
  (compose replace-quote replace-slash replace-hex))

(printf "Part one: ~a\n" (sum (for/list ([line lines])
                                (- (string-length line)
                                   (- (string-length (replace-escapees line)) 2)))))

;; Part Two
(define (replace-quote^ str)
  (regexp-replace* #rx"\"" str "\\\\\""))
(define (replace-slash^ str)
  (regexp-replace* #rx"\\\\" str "\\\\\\\\"))
(define replace-escapees^
  (compose replace-quote^ replace-slash^))

(printf "Part two: ~a\n" (sum (for/list ([line lines])
                                (- (+ (string-length (replace-escapees^ line)) 2)
                                   (string-length line)))))
