#lang racket

(require "utils.rkt")

(day 23)

(struct instr (n a1 a2) #:transparent)

;; Part One
(define (parse-instrs lines)
  (for/hash ([i (range (length lines))]
             [line lines])
    (match-let ([(list _ n a1 _ a2 _)
                 (regexp-match #px"(hlf|tpl|inc|jmp|jie|jio) (a|b|(\\+|\\-)\\d+),? ?((\\+|\\-)\\d+)?" line)])
      (values i (instr n a1 a2)))))

(define (run-program instrs (pc 0) (regs (dict-set* (make-immutable-hash) "a" 0 "b" 0)))
  (define i (dict-ref instrs pc #f))
  (match i
    (#f regs)
    ((instr "hlf" r _) (run-program instrs (add1 pc) (dict-set regs r (quotient (dict-ref regs r) 2))))
    ((instr "tpl" r _) (run-program instrs (add1 pc) (dict-set regs r (* (dict-ref regs r) 3))))
    ((instr "inc" r _) (run-program instrs (add1 pc) (dict-set regs r (add1 (dict-ref regs r)))))
    ((instr "jmp" r _) (run-program instrs (+ pc (string->number r)) regs))
    ((instr "jie" r o) (run-program instrs (if (even? (dict-ref regs r))
                                               (+ pc (string->number o)) (add1 pc)) regs))
    ((instr "jio" r o) (run-program instrs (if (equal? 1 (dict-ref regs r))
                                               (+ pc (string->number o)) (add1 pc)) regs))))

(define instrs (parse-instrs lines))
(printf "Part one: ~a\n" (dict-ref (run-program instrs) "b"))

;; Part Two
(printf "Part two: ~a\n" (dict-ref (run-program instrs 0 (dict-set* (make-immutable-hash) "a" 1 "b" 0)) "b"))
