#lang racket

(require "utils.rkt")

(day 19)

(define-values (rs ms) (splitf-at lines (λ (s) (not (equal? 0 (string-length s))))))
(define medicine (last ms))

;; Part One
(define (parse-reps rs)
  (for/fold ([rd (make-immutable-hash)]) ([r rs])
    (match-let ([(list _ s d) (regexp-match #px"([a-zA-Z]+) => ([a-zA-Z]+)" r)])
      (dict-set rd s (cons d (dict-ref rd s '()))))))

(define (parse-medicine m)
  (regexp-match* #px"(e|[A-Z][a-z]*)" m))


(define (make-reps med rs)
  (match med
    ('() (set))
    ((cons m med)
     (set-union (for/set ([r (dict-ref rs m '())])
                  (string-append r (string-join med "")))
                (set-map->set (curry string-append m) (make-reps med rs))))))

(printf "Part one: ~a\n" (set-count (make-reps (parse-medicine medicine) (parse-reps rs))))

;; Part Two
(define (inverse-reps rs)
  (for*/hash ([(s ds) rs]
              [d ds])
    (values d s)))

(define (split-medicine med irs)
  (regexp-match-positions* (pregexp (string-append "(" (string-join (dict-keys irs) "|") ")")) med))

(define (string-replace-idxs str rep s e)
  (string-append (substring str 0 s) rep (substring str e)))

;;; not used as it takes to long.
;;; general solution.
(define (bfs irs goal (visited (set)) (fringe (list (cons medicine 0))))
  (match fringe
    ('() -1)
    ((cons (cons med cost) fringe)
     (cond
       ((equal? goal med) cost)
       ((set-member? visited med) (bfs irs goal visited fringe))
       (else (define splits (split-medicine med irs))
             (bfs irs goal (set-add visited med)
                  (append fringe (set-map (for/set ([split splits])
                                            (match-let* ([(cons s e) split]
                                                         [substr (substring med s e)])
                                              (string-replace-idxs med (dict-ref irs substr) s e)))
                                          (λ (v) (cons v (add1 cost)))))))))))

;; Randomly apply each transformation all at once.
;; If it works keeping going therwise restart.
;; This must be something to do with input not
;; being general.
(define med-start medicine)
(define (blah med irs cnt)
  (cond
    ((equal? med "e") cnt)
    (else
     (define-values (med^ cnt^)
       (for/fold ([med^ med] [cnt^ 0])
                 ([from (shuffle (dict-keys irs))])
         (values (string-replace med^ from (dict-ref irs from))
                 (+ cnt^ (length (regexp-match* from med^))))))
     (if (not (equal? med med^))
         (blah med^ irs (+ cnt cnt^))
         (blah med-start irs 0)))))

(printf "Part two: ~a\n" (blah medicine (inverse-reps (parse-reps rs)) 0))
