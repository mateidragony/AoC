 #lang racket

(define year 0000)
(define input-directory "inputs")
(define test-file-name "in-test.txt")

(require net/url)
(require racket/format)

(require "private.rkt")

(provide
 ;; input api
 lines chars rows cols day testing
 ;; general utils
 sum char-num? b->n add-points symbol-append number->symbol
 ls-of sub-list 2d-ref 2d-vec-copy 2d-set! sub-vec
 in-bounds dict-filter dict-append flip-dict dict-max-k/v
 set-map->set set-filter set-filter-map set-flatten
 uniquify-name get-or-default
 (struct-out point)
 (struct-out Just)
 (struct-out None))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract (sum ls)
  (-> (listof integer?) integer?)
  (foldr + 0 ls))
(define/contract (char-num? c)
  (-> char? boolean?)
  (char<=? #\0 c #\9))
(define/contract (b->n b)
  (-> boolean? integer?)
  (if b 1 0))

;; points
(struct point (x y) #:transparent)
(define/contract (add-points p1 p2)
  (-> point? point? point?)
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-y p2))))

;; Extra symbol functions
(define/contract (symbol-append s1 s2)
  (-> symbol? symbol? symbol?)
  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))
(define/contract (number->symbol n)
  (-> number? symbol?)
  (string->symbol (number->string n)))

;; Extra list functions
(define/contract (ls-of l e)
  (-> integer? any/c list?)
  (build-list l (const e)))
(define/contract (sub-list l s e)
  (-> list? integer? integer? list?)
  (drop (take l e) s))

;; Extra vector functions
(define/contract (2d-ref m i j [d #f])
  (-> (vectorof (vectorof any/c)) integer? integer? any/c any/c)
  (if (in-bounds m i j) (vector-ref (vector-ref m i) j) d))
(define/contract (2d-vec-copy m)
  (-> (vectorof (vectorof any/c)) (vectorof (vectorof any/c)))
  (for/vector ([v m]) (vector-copy v)))
(define/contract (2d-set! m i j v)
  (-> (vectorof (vectorof any/c)) integer? integer? any/c void)
  (vector-set! (vector-ref m i) j v))
(define/contract (sub-vec v si ee)
  (-> vector? integer? integer? vector?)
  (vector-drop (vector-take v ee) si))
(define/contract (in-bounds m i j)
  (-> (vectorof (vectorof any/c)) integer? integer? boolean?)
  (and (0 . <= . i) (i . < . (vector-length m))
       (0 . <= . j) (j . < . (vector-length (vector-ref m 0)))))


;; Extra dict functions
(define/contract (dict-filter p d)
  (-> (-> any/c any/c boolean?) dict? dict?)
  (cond ((dict-empty? d) d)
        (else (define k (car (dict-keys d)))
              (if (p k (dict-ref d k))
                  (dict-set (dict-filter p (dict-remove d k)) k (dict-ref d k))
                  (dict-filter p (dict-remove d k))))))
(define/contract (dict-append . ds)
  (-> dict? ... dict?)
  ;; Combine the values of a key from two dictionaries.
  ;; If the values are lists, append the two lists.
  ;; If the values are sets, union the two sets.
  ;; Otherwise return the value from the first dict.
  (define/contract (dict-combine-key k d1 d2)
    (-> any/c dict? dict? any/c)
    (define v1 (dict-ref d1 k))
    (define v2 (dict-ref d2 k))
    (cond
      ((list? v1) (append v1 v2))
      ((set?  v1) (set-union v1 v2))
      (else v1)))
  ;; Append two dictionaries. if there are key collisions, then
  ;; combine the values of two keys
  (define/contract (dict-append-two d1 d2)
    (-> dict? dict? dict?)
    (cond
      ((dict-empty? d1) d2)
      (else (define k (car (dict-keys d1)))
            (dict-append-two (dict-remove d1 k) (if (dict-has-key? d2 k)
                                                    (dict-set d2 k (dict-combine-key k d1 d2))
                                                    (dict-set d2 k (dict-ref d1 k)))))))
  (match ds
    ['() '()]
    [(cons d '()) d]
    [(cons d ds) (dict-append-two d (apply dict-append ds))]))
(define/contract (flip-dict d empty-dict)
  (-> dict? dict? dict?)
  (foldr (λ (x d) (dict-set d (cdr x) (car x))) empty-dict (dict->list d)))
(define/contract (dict-max-k/v d fn)
  (-> dict? (-> any/c any/c boolean?) (values any/c any/c))
  (for/fold ([km (first (dict-keys d))]
             [vm (dict-ref d (first (dict-keys d)))])
            ([k (dict-keys d)])
    (if (fn (dict-ref d k) vm)
        (values k (dict-ref d k))
        (values km vm))))

;; Extra set functions
(define/contract (set-map->set proc st)
  (-> (-> any/c any/c) set? set?)
  (for/set ([x st]) (proc x)))
(define/contract (set-filter proc st)
  (-> (-> any/c boolean?) set? set?)
  (for/fold ([fs (set)]) ([x st]) (if (proc x) (set-add fs x) fs)))
(define/contract (set-filter-map proc st)
  (-> (-> any/c any/c) set? set?)
  (for/fold ([fs (set)]) ([x st]) (let ((px (proc x))) (if px (set-add fs px) fs))))
(define/contract (set-flatten st)
  (-> set? set?)
  (for/fold ([fl (set)]) ([x st]) (set-union fl (if (set? x) (set-flatten x) (set x)))))

;; Uniquification
(define unique-number 0)
(define/contract (uniquify-name x)
  (-> symbol? symbol?)
  (begin
    (set! unique-number (add1 unique-number))
    (symbol-append x (symbol-append (string->symbol ".") (number->symbol unique-number)))))

;; Maybe
(struct Just (v) #:transparent)
(struct None () #:transparent)
(define/contract (maybe? x)
  (-> any/c boolean?)
  (match x
    ((Just _) #t)
    ((None) #t)
    (_ #f)))
(define/contract (get-or-default m d)
  (-> maybe? any/c any/c)
  (match m
    ((Just x) x)
    ((None) d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input functions (Thanks calcin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract (make-input-request day)
  (-> integer? string?)
  (port->string (get-pure-port (string->url (format "https://adventofcode.com/~a/day/~a/input" year day))
                               (list (format "Cookie: ~a" PRIV_COOKIE)))))

(define/contract (pad-day day)
  (-> integer? string?)
  (~a day #:min-width 2 #:align 'right #:left-pad-string "0"))

(define/contract (get-input-path day)
  (-> integer? string?)
  (format "~a/in_~a.txt" input-directory (pad-day day)))

(define/contract (get-input day testing?)
  (-> integer? boolean? (listof string?))
  (define fname (get-input-path day))
  (when (not (directory-exists? input-directory))
    (make-directory input-directory))
  (cond
    ((or testing? (< day 1) (> day 25))
     (file->lines (format "~a/~a" input-directory test-file-name)))
    ((file-exists? fname)
     (file->lines fname))
    (else
     (define raw (make-input-request day))
     (display-to-file raw fname)
     (string-split raw "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-input)
  (set! lines (get-input day-num testing?))
  (set! chars (list->vector (map (compose list->vector string->list) lines)))
  (set! rows (vector-length chars))
  (set! cols (vector-length (vector-ref chars 0))))

(define day-num 1)
(define testing? #f)

(define lines #f)
(define chars #f)
(define rows  #f)
(define cols  #f)
(update-input)

(define/contract (day n)
  (-> integer? void)
  (set! day-num n)
  (update-input))

(define/contract (testing b)
  (-> boolean? void)
  (set! testing? b)
  (update-input))
