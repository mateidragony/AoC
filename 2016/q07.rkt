#lang racket

(require "utils.rkt")

(day 7)
(testing #f)


;; Part One
(define (get-abbas str)
  (regexp-match* #px"([a-z])([a-z])(?!\\1)\\2\\1" str))

(define (hypernet-abbas? str)
  (regexp-match #px"[^\\[]*\\[[^\\]]*(([a-z])([a-z])(?!\\2)\\3\\2)[^\\]]*\\][^\\[]*" str))

(define (tls? str)
  (and (not (hypernet-abbas? str))
       (not (empty? (get-abbas str)))))

(printf "Part one: ~a\n" (length (filter tls? lines)))

;; Part Two
(define (ssl? str)
  (define supernets (regexp-split #px"\\[[^\\]]*\\]" str))
  (define hypernets (regexp-match* #px"\\[[^\\]]*\\]" str))
  (define abas (flatten (map (curry regexp-match*-overlapping #px"([a-z])(?!\\1)([a-z])\\1") supernets)))
  (define babs (flatten (map (curry regexp-match*-overlapping #px"([a-z])(?!\\1)([a-z])\\1") hypernets)))
  (for/or ([aba abas])
    (member (list->string (list (string-ref aba 1) (string-ref aba 0) (string-ref aba 1))) babs)))

(printf "Part two: ~a\n" (length (filter ssl? lines)))
