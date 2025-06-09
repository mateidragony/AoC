#lang racket

(require "utils.rkt")

(day 24)

(define packages (map string->number lines))

;; Part One
(define (possible-sums ls s)
  (match ls
    ('() #:when (not (zero? s)) '())
    ('() '(()))
    ((cons n ls) #:when (<= n s)
                 (define reti (possible-sums ls (- s n)))
                 (if (empty? reti)
                     (possible-sums ls s)
                     (append (map (curry cons n) reti)
                             (possible-sums ls s))))
    ((cons n ls) (possible-sums ls s))))

(define (partition-packages pss (min-ps (length packages)) (mins '()) (groups 3))
  (match pss
    ('() mins)
    ((cons ps _) #:when (< min-ps (length ps)) mins)
    ((cons ps pss)
     (define-values (possible? _) (for/fold ([possible? #t]
                                             [ls (remove* ps packages)])
                                            ([_ (range (- groups 2))]) ;; already did one and I dont need to check last
                                    (define poss (possible-sums ls (quotient (sum packages) groups)))
                                    (values (and possible? (not (empty? poss))) (remove* poss ls))))
     (cond
       ((and possible? (< (length ps) min-ps)) (partition-packages pss (length ps) (list ps) groups))
       (possible? (partition-packages pss min-ps (cons ps mins) groups))
       (else (partition-packages pss min-ps mins groups))))))

(printf "Part one: ~a\n" (apply min (for/list ([ps (partition-packages (sort
                                                                        (possible-sums packages (quotient (sum packages) 3))
                                                                        (λ (a b) (< (length a) (length b)))))])
                                      (apply * ps))))

;; Part Two
(printf "Part two: ~a\n" (apply min (for/list ([ps (partition-packages
                                                    (sort
                                                     (possible-sums packages (quotient (sum packages) 4))
                                                     (λ (a b) (< (length a) (length b))))
                                                    (length packages)
                                                    '()
                                                    4)])
                                      (apply * ps))))
