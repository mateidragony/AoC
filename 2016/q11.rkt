#lang racket

(require graph)
(require "utils.rkt")

(day 11)
(testing #t)

(struct rtg/chip (element rtg?) #:transparent)

(define floors-test (list 
                     (set (rtg/chip "H" #f) (rtg/chip "L" #f)) 
                     (set (rtg/chip "H" #t)) 
                     (set (rtg/chip "L" #t))
                     (set)))

(define floors (list
                (set (rtg/chip "T" #t) (rtg/chip "T" #f) (rtg/chip "Pl" #t) (rtg/chip "S" #t))
                (set (rtg/chip "Pl" #f) (rtg/chip "S" #f))
                (set (rtg/chip "Pr" #t) (rtg/chip "Pr" #f) (rtg/chip "R" #t) (rtg/chip "R" #f))
                (set)))

;; Part One
(define (floor-valid? f [microchips '()] [generators '()])
  (match f
    ('() (or (null? generators)
             (and (null? microchips) (not (null? generators)))))
    ((cons (rtg/chip element rtg?) f)
     (cond
       ((and rtg? (member element microchips)) (floor-valid? f (remove element microchips)
                                                             generators))
       (rtg? (floor-valid? f microchips (cons element generators)))
       ((member element generators) (floor-valid? f (remove element microchips) generators))
       (else (floor-valid? f (cons element microchips) generators))))))

(define (floors-valid? floors)
  (for/and ([f floors])
    (floor-valid? (set->list f))))

(define (assembly-done? floors)
  (and (set-empty? (first floors))
       (set-empty? (second floors))
       (set-empty? (third floors))))

(define (generate-valid-steps-up floors elevator-level)
  (define cur-level (list-ref floors elevator-level))
  (define one-comb (combinations (set->list cur-level) 1))
  (define two-comb (combinations (set->list cur-level) 2))
  (filter floors-valid?
          (if (< elevator-level 3)
              (for/list ([comb (append one-comb two-comb)])
                (list-set (list-set floors elevator-level (set-subtract cur-level (list->set comb)))
                          (add1 elevator-level)
                          (set-union (list->set comb) (list-ref floors (add1 elevator-level)))))
              '())))

(define (floors-below-empty? floors elevator-level)
  (for/and ([i (range elevator-level)])
    (set-empty? (list-ref floors i))))

(define (generate-valid-steps-dn floors elevator-level)
  (define cur-level (list-ref floors elevator-level))
  (define one-comb (combinations (set->list cur-level) 1))
  (define two-comb (combinations (set->list cur-level) 2))
  (cond
    ((equal? elevator-level 0) '())
    ((floors-below-empty? floors elevator-level) '())
    ((> elevator-level 0)
      (filter floors-valid?
              (for/list ([comb (append one-comb two-comb)])
                (list-set (list-set floors elevator-level (set-subtract cur-level (list->set comb)))
                          (sub1 elevator-level)
                          (set-union (list->set comb) (list-ref floors (sub1 elevator-level)))))))))

(define (print-floors floors elevator-level)
  (printf (floors->string floors elevator-level)))

(define (floors->string floors elevator-level)
  (with-output-to-string
    (lambda ()
      (for ([i (reverse (range 4))])
        (define f (list-ref floors i))
        (printf "F~a " (add1 i))
        (when (equal? i elevator-level)
          (printf "E "))
        (for ([item f])
          (if (rtg/chip-rtg? item)
              (printf "~aG " (rtg/chip-element item))
              (printf "~aM " (rtg/chip-element item))))
        (printf "\n"))
      (printf "\n"))))

(define (print-steps elevator-level floors)
  (for ([step (append (generate-valid-steps-up floors elevator-level)
                      (generate-valid-steps-dn floors elevator-level))])
    (for ([f step])
      (printf "~a\n" f))
    (printf "\n" )))


(define (print-path node)
  (match node
    ((fringe-node floors elevator-level step-num prev)
     (printf "~a\n" step-num)
     (print-floors floors elevator-level)
     (print-path prev))
    (_ (printf "\n"))))

(struct fringe-node (floors elevator-level step-num prev) #:transparent)
(define memo (make-hash))

(define (bfs (fringe (list (fringe-node floors 0 0 null))))
  (match fringe
    ('() -1)
    ((cons (fringe-node floors elevator-level step-num prev) fringe)
     (cond
       ((assembly-done? floors)
        step-num)
       ((hash-has-key? memo (cons elevator-level floors)) (bfs fringe))
       (else
        (hash-set! memo (cons elevator-level floors) -1)
        (define steps (append
                       (for/list ([step (generate-valid-steps-up floors elevator-level)])
                         (fringe-node step (add1 elevator-level) (add1 step-num)
                                      (fringe-node floors elevator-level step-num prev)))
                       (for/list ([step (generate-valid-steps-dn floors elevator-level)])
                         (fringe-node step (sub1 elevator-level) (add1 step-num)
                                      (fringe-node floors elevator-level step-num prev)))))
        (bfs (append fringe steps)))))))

(printf "Part one: ~a\n" (bfs))

;; part Two
(define floors-pt2 (list
                    (set (rtg/chip "T" #t) (rtg/chip "T" #f) (rtg/chip "Pl" #t) (rtg/chip "S" #t)
                         (rtg/chip "E" #t) (rtg/chip "E" #f) (rtg/chip "D" #t) (rtg/chip "D" #f))
                    (set (rtg/chip "Pl" #f) (rtg/chip "S" #f))
                    (set (rtg/chip "Pr" #t) (rtg/chip "Pr" #f) (rtg/chip "R" #t) (rtg/chip "R" #f))
                    (set)))

(printf "Part two: ~a\n" (bfs (list (fringe-node floors-pt2 0 0 null))))
