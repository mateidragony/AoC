 #lang racket

(require graph)
(require "utils.rkt")

(day 7)

(struct gate (inputs fn) #:transparent)

;; Part One
(define (make-gates lines)
  (for/hash ([line lines])
    (define init   (regexp-match #px"^([a-zA-Z]+|\\d+) -> ([a-zA-Z]+)$" line))
    (define unary  (regexp-match #px"^(NOT) ([a-zA-Z]+|\\d+) -> ([a-zA-Z]+)$" line))
    (define binary (regexp-match #px"^([a-zA-Z]+|\\d+) (AND|OR|LSHIFT|RSHIFT) ([a-zA-Z]+|\\d+) -> ([a-zA-Z]+)$" line))
    (cond
      (init   (values (third init)   (gate (list (second init)) "INIT")))
      (unary  (values (fourth unary) (gate (list (third unary)) (second unary))))
      (binary (values (fifth binary) (gate (list (second binary) (fourth binary)) (third binary)))))))

(define (get-dependencies gate)
  (filter (negate string->number) (gate-inputs gate)))

(define (make-graph gates)
  (define g (unweighted-graph/directed '()))
  (for* ([(k gate) gates]
         [dep (get-dependencies gate)])
    (add-directed-edge! g dep k))
  g)

(define gates (make-gates lines))
(define g (make-graph gates))

(define (rshift a b)
  (arithmetic-shift a (* -1 b)))

(define (eval-gate gate vals)
  (apply (match (gate-fn gate)
           ("INIT"   id)
           ("NOT"    bitwise-not)
           ("AND"    bitwise-and)
           ("OR"     bitwise-ior)
           ("LSHIFT" arithmetic-shift)
           ("RSHIFT" rshift))
         (map (λ (v) (if (string->number v) (string->number v) (dict-ref vals v)))
              (gate-inputs gate))))

(define (eval-gates order gates (vals (make-immutable-hash)))
  (match order
    ('() vals)
    ((cons k order)
     (eval-gates order gates (dict-set vals k (eval-gate (dict-ref gates k) vals))))))

(define part1 (dict-ref (eval-gates (tsort g) gates) "a"))
(printf "Part one: ~a\n" part1)

;; Part Two
(printf "Part two: ~a\n" (dict-ref (eval-gates
                                    (remove "b" (tsort g)) gates
                                    (dict-set (make-immutable-hash) "b" part1))
                                   "a"))
