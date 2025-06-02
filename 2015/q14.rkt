#lang racket

(require "utils.rkt")

(day 14)

(struct reindeer (n s sd rd d ) #:transparent)

;; Part One
(define (parse-reindeer lines)
  (for/list ([line lines])
    (match-let ([(list _ n s sd rd) (regexp-match #px"([a-zA-Z]+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds." line)])
      (reindeer n (string->number s) (string->number sd) (string->number rd) 0 ))))

(define reindeers (parse-reindeer lines))

(define (travel-n n r)
  (define period (+ (reindeer-sd r) (reindeer-rd r)))
  (+ (* (reindeer-sd r) (quotient n period) (reindeer-s r))
     (* (min (modulo n period) (reindeer-sd r)) (reindeer-s r))))

(printf "Part one: ~a\n" (apply max (map (curry travel-n 2503) reindeers)))

;; Part Two

(define (travel t r)
  (define period (+ (reindeer-sd r) (reindeer-rd r)))
  (if (< (modulo t period) (reindeer-sd r))
      (reindeer (reindeer-n r) (reindeer-s r) (reindeer-sd r) (reindeer-rd r) (+ (reindeer-s r) (reindeer-d r)))
      r))

(define (update-scores rs best scores)
  (match rs
    ('() scores)
    ((cons r rs) (update-scores rs best
                                (if (equal? best (reindeer-d r))
                                    (dict-set scores (reindeer-n r)
                                          (add1 (dict-ref scores (reindeer-n r) 0)))
                                    scores)))))

(define (race n rs (c 0) (scores (make-immutable-hash)))
  (cond
    ((equal? n  c) scores)
    (else (let* ((rs (map (curry travel c) rs))
                 (best-d (reindeer-d (first (sort rs (λ (a b) (> (reindeer-d a) (reindeer-d b))))))))
            (race n rs (add1 c) (update-scores rs best-d scores))))))

(printf "Part two: ~a\n" (apply max (dict-values (race 2503 reindeers))))
