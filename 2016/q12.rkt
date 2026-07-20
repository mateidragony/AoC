#lang racket

(require "utils.rkt")

(day 12)
(testing #f)

;; Part One
(struct instr (type arg1 arg2) #:transparent)

(define (parse-instrs lines)
  (for/list ([line lines])
    (match-let ([(list _ type arg1 arg2)
                 (regexp-match #px"(cpy|inc|dec|jnz) (a|b|c|d|\\d+) ?(a|b|c|d|-?\\d+)?" line)])
      (instr type (if (string->number arg1) (string->number arg1) arg1)
             (if (and arg2 (string->number arg2)) (string->number arg2) arg2)))))

(define (execute-code instrs (pc 0) (regs (dict-set* (make-immutable-hash)
                                                          "a" 0 "b" 0 "c" 0 "d" 0)))
  (if (equal? (length instrs) pc)
      (dict-ref regs "a")
      (match (list-ref instrs pc)
        ((instr "cpy" a1 a2) #:when (number? a1) (execute-code instrs (add1 pc)
                                                               (dict-set regs a2 a1)))
        ((instr "cpy" a1 a2) (execute-code instrs (add1 pc) (dict-set regs a2 (dict-ref regs a1))))
        ((instr "inc" a1 _) (execute-code instrs (add1 pc) (dict-set regs a1
                                                                     (add1 (dict-ref regs a1)))))
        ((instr "dec" a1 _) (execute-code instrs (add1 pc) (dict-set regs a1
                                                                     (sub1 (dict-ref regs a1)))))
        ((instr "jnz" a1 a2) (if (zero? (if (number? a1) a1 (dict-ref regs a1)))
                                 (execute-code instrs (add1 pc) regs)
                                 (execute-code instrs (+ pc a2) regs))))))


(printf "Part one: ~a\n" (execute-code (parse-instrs lines)))

;; Part Two
(printf "Part two: ~a\n" (execute-code (parse-instrs lines) 0 (dict-set* (make-immutable-hash)
                                                                         "a" 0 "b" 0 "c" 1 "d" 0)))
