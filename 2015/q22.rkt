#lang racket

(require "utils.rkt")

(day 22)

(struct boss (hp d) #:transparent)
(struct player (hp m a) #:transparent)
(struct effect (n d) #:transparent)
(struct spell (n c) #:transparent)

(define min-mana 53)
(define spells (list (spell 'missile 53)
                     (spell 'drain 73)
                     (spell 'shield 113)
                     (spell 'poison 173)
                     (spell 'recharge 229)))

(define (boss-attack p b)
  (struct-copy player p [hp (- (player-hp p) (max 1 (- (boss-d b) (player-a p))))]))

(define (apply-effect e p b)
  (match e
    ((effect 'shield _) (values p b))
    ((effect 'poison _) (values p (struct-copy boss b [hp (- (boss-hp b) 3)])))
    ((effect 'recharge _) (values (struct-copy player p [m (+ (player-m p) 101)]) b))))

(define (apply-effects es p b)
  (for/fold ([p^ p] [b^ b])
            ([e es])
    (apply-effect e p^ b^)))

(define (cast-spell s p b)
  (match s
    ((spell 'missile c)  (values (struct-copy player p [m (- (player-m p) c)])
                                 (struct-copy boss b [hp (- (boss-hp b) 4)])))
    ((spell 'drain c)    (values (struct-copy player p [hp (+ (player-hp p) 2)] [m (- (player-m p) c)])
                                 (struct-copy boss b [hp (- (boss-hp b) 2)])))
    ((spell 'shield c)   (values (struct-copy player p [m (- (player-m p) c)])
                                 b))
    ((spell 'poison c)   (values (struct-copy player p [m (- (player-m p) c)])
                                 b))
    ((spell 'recharge c) (values (struct-copy player p [m (- (player-m p) c)])
                                 b))))

(define (active-effects es)
  (filter (compose not zero? effect-d) es))

(define (castable-spells es p)
  (filter (λ (s)
            (and (<= (spell-c s) (player-m p))
                 (not (member (spell-n s) (map effect-n es)))))
          spells))

(define (spell->effect s)
  (match s
    ((spell 'shield _) (list (effect 'shield 6)))
    ((spell 'poison _) (list (effect 'poison 6)))
    ((spell 'recharge _) (list (effect 'recharge 5)))
    (else '())))

(define (update-effects es)
  (map (λ (e) (struct-copy effect e [d (sub1 (effect-d e))])) es))

(define (sum-mana spells)
  (sum (map spell-c spells)))

(define (minimize-mana spellss)
  (for/fold ([least #f])
            ([spells spellss])
    (cond
      ((not least) spells)
      ((and spells (< (sum-mana spells) (sum-mana least))) spells)
      (else least))))

(define memo (make-hash))
(define (run pp b (es '()) (turn 0) (hard? #f))
  (define key (list pp b es))
  (define p (if (and hard? (even? turn)) (struct-copy player pp [hp (sub1 (player-hp pp))]) pp))
  (cond
    ((<= (player-hp p) 0) #f)
    ((and (even? turn) (<  (player-m p) min-mana)) #f)
    ((<= (boss-hp b) 0) '())
    ((dict-has-key? memo key) (dict-ref memo key))
    (else
     (define-values (p^ b^) (apply-effects es p b))
     (define es^ (active-effects (update-effects es)))
     (cond
       ((even? turn)
        (define ret-min
          (minimize-mana (for/list ([s (castable-spells es^ p^)])
                           (define-values (p^^ b^^) (cast-spell s p^ b^))
                           (define ret (run (struct-copy player p^^ [a (if (member 'shield (map effect-n es)) 7 0)])
                                            b^^ (append (spell->effect s) es^) (add1 turn) hard?))
                           (and ret (cons s ret)))))
        (dict-set! memo key ret-min)
        ret-min)
       (else (run (boss-attack p^ b^) b^ es^ (add1 turn) hard?))))))

;; Part One
(define (parse-input lines)
  (match-let ([(list _ hp) (regexp-match #px"Hit Points: (\\d+)" (first lines))]
              [(list _ d)  (regexp-match #px"Damage: (\\d+)" (second lines))])
    (values (string->number hp) (string->number d))))


(define base-hp 50)
(define base-mana 500)
(define-values (boss-hitpoints boss-damage) (parse-input lines))

(printf "Part one: ~a\n" (sum-mana (run (player base-hp base-mana 0) (boss boss-hitpoints boss-damage))))

;; Part Two
(dict-clear! memo)
(printf "Part two: ~a\n" (sum-mana (run (player base-hp base-mana 0) (boss boss-hitpoints boss-damage) '() 0 #t)))
