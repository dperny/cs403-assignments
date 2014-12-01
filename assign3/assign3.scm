;
; CS 403
; Assignment 3
; Drew Erny
;
; Checklist:
;   [ ] Task 1
;   [ ] Task 2
;   [ ] Task 3
;   [ ] Task 4
;   [ ] Task 5
;   [ ] Task 6
;   [x] Task 7
;   [x] Task 8
;   [ ] Task 9
;   [ ] Task 10

(define (author)
  (println "AUTHOR: Drew Erny dperny@crimson.ua.edu")
  )

;==============================================================================
; Task 1
;==============================================================================

;
; overloads set!
;
; params:
;   $obj - the variable to be set
;   val  - the value to set it to
;   @    - option: an environment to set in
; return:
;   undefined
(define (set! # $obj val @)
  ; (define env (if (null? @) # (car @)))
  (define (helper env)
    (if (cddr env ()))
    )
  )

(define (NOrun1)
  ()
  )

;==============================================================================
; Task 2
;==============================================================================



;==============================================================================
; Task 3
;==============================================================================

;
; Creates a new priority queue object
;
; params:
;   comparator - a function to compare objects being inserted that returns true
;                if the first item comes before the other in the ordering
; return:
;   the priority queue object
(define (PRQ comparator)
  
  ; private fields. get off my lawn
  (define _size 0)
  (define _capacity 8)
  (define _comparator comparator)


  ; the store is a linked list with the cars consisting of cells in the form
  ;   (cons rank item)
  ;
  ; enqueues to the front of _store
  (define _store nil)

  ; self is a reference to this object always everywhere
  (define self this)

  ;
  ; inserts an item into the priority queue
  ;
  ; params:
  ;   item - the item to be inserted
  ;   rank - the rank
  (define (insert item rank)
    (define (help store)
      (cond
        (else nil)
        )
      )
    )

  ;
  ; dequeues the pending item, removing it from the structure
  ;
  ; return:
  ;   the item at the top of the priority queue
  (define (remove)
    ()
    )

  ;
  ; peeks at the top item, but does not remove it
  ;
  ; return:
  ;   the item at the top of the priority queue
  (define (item)
    ()
    )

  ; 
  ; shows the rank of the top item, but does not remove it.
  ;
  ; return:
  ;   the rank of the item at the top of the priority queue
  (define (rank)
    ()
    )

  ;
  ; gets the number of ites in the priority queue
  ;
  ; return:
  ;   the size of the priority queue
  (define (size)
    _size
    )

  ;
  ; returns if the priority queue is empty or not
  ;
  ; return:
  ;   #t if the queue is empty, #f otherwise
  (define (empty?)
    (= _size 0)
    )

  this
  )

;==============================================================================
; Task 4
;==============================================================================


;============================================================================== 
; STREAM FUNCTIONS. FOR YOUR HEALTH
;==============================================================================

; some shorthands
(define scar stream-car)
(define scdr stream-cdr)

; 
; displays the first n items in a stream
;
; params:
;   s - the stream to display
;   n - the number of items to display
; returns:
;   'OK upon completion
;
(define (stream-display s n)
  (define (helper t c)
    (if (= c 0)
      'OK
      (begin
        (print (stream-car t) " ")
        (helper (stream-cdr t) (- c 1))
        )
      )
    )
  (print "(")
  (helper s n)
  (println "...)")
  )

;
; Gets the nth value of a stream
;
; params:
;   s - the stream 
;   n - the term wanted
; returns:
;   a single value representing the nth element in the stream
;
(define (stream-ref s n)
  (define (helper t c store)
    (if (= c 0)
      store
      (helper (stream-cdr t) (- c 1) (scar t))
      )
    )
  (helper s n (scar s))
  )

; 
; creates a stream that repeats a given figure
;
; params:
;   x - the figure to repeat
; returns:
;   a stream repeating x
;
(define (repeat x)
  (cons-stream x (repeat x))
  )

;
; maps a function onto a stream
;
; params:
;   f - the function to map to the stream
;   s - the stream to be mapped
; returns:
;   a stream with f applied to each element of s
;
(define (stream-map f s)
  (cons-stream
    (f (stream-car s))
    (stream-map f (stream-cdr s))
    )
  )

;
; keeps certain elements of a stream
;
; params:
;   p? - a predicate function returning true if an item is to be kept
;   s  - the to apply to
; returns:
;   a stream with only elements where p? is true
;
(define (stream-keep p? s)
  (if (p? (scar s))
    (cons-stream (scar s) (stream-keep p? (scdr s)))
    (stream-keep p? (scdr s))
    )
  )

;
; merges two streams shufflewise, element by element
;
; params:
;   s - the stream who's element comes first
;   t - the stream who's element comes next
; returns:
;   a stream which alternates elements from s and t
;
(define (stream-merge s t)
  (cons-stream
    (stream-car s)
    (stream-merge (scdr t) (scdr s))
    )
  )

;
; 
(define (stream-multiply s t)
  (cons-stream
    (* (stream-car s) (stream-car t))
    (stream-multiply (stream-cdr s) (stream-cdr t))
    )
  )

(define (stream-add s t)
  (cons-stream
    (+ (stream-car s) (stream-car t))
    (stream-add (stream-cdr s) (stream-cdr t))
    )
  )

;
; creates a stream representing successive partial sums from elements of 
; another stream
;
; params:
;   s - a stream to be summed
; returns:
;   a stream representing successive sums
;
(define (partial-sum s)
  (cons-stream
    (scar s)
    (stream-add (scdr s) (partial-sum s))
    )
  )

;==============================================================================
; Task 6
;==============================================================================

(define sevens (repeat 7))
(define elevens (repeat 11))

(define pow7 (cons-stream 7 (stream-multiply sevens pow7)))
(define pow11 (cons-stream 11 (stream-multiply elevens pow11)))

(define big-gulp
  sevens
  )

(define (NOrun6)
  (stream-display sevens 3)
  (stream-display elevens 3)
  (stream-display (stream-merge sevens elevens) 4)
  (stream-display pow7 3)
  (stream-display pow11 3)
  ; (stream-display big-gulp 12)
  )

;==============================================================================
; Task 7 - DONE
;==============================================================================

(define countingNums
  (cons-stream 1 (stream-add (repeat 1) countingNums))
  )

(define (divisible? a b) (= (% a b) 0))

(define (sieve stream)
  (cons-stream
    (scar stream)
    (sieve 
      (stream-keep
        (lambda (x) (not (divisible? x (scar stream))))
        (scdr stream)
        )
      )
    )
  )

(define primes (sieve (scdr countingNums)))

(define (pair stream)
  (if (= (- (scar (scdr stream)) (scar stream)) 2)
    (cons-stream
      (list (scar stream) (scar (scdr stream)))
      (pair (scdr stream))
      )
    (pair (scdr stream))
    )
  )

(define twinPrimes (pair primes))
  

(define (run7)
  ; (stream-display countingNums 3)
  ; (stream-display primes 7)
  (println "first seven twin primes?")
  (stream-display twinPrimes 7)
  )

;==============================================================================
; Task 8 - DONE
;==============================================================================

(define (square x) (* x x))
(define tiny .00000000000000000000000000000000000000000001)
; 
; Performs an euler transform on a sequence of appropriate form
;
; params:
;   s - a stream to transform
; returns:
;   a stream euler-transformed to converge faster
;
(define (euler-transform s)
  (let* ((s0 (scar s))                  ; Sn-1
        (s1 (scar (scdr s)))           ; Sn
        (s2 (scar (scdr (scdr s))))   ; Sn+1
        (divisor
          (if (= 0 (+ s0 (* -2 s1) s2))
            tiny
            (+ s0 (* -2 s1) s2)
            )
          ))
    (cons-stream 
      (- s2 (/ (^ (- s2 s1) 2) divisor))
      (euler-transform (stream-cdr s))))
  )

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (get-convergence s)
  (define (helper t store count)
    (if (> tiny (abs (- (scar t) store)))
      count
      (helper (scdr t) (scar t) (+ count 1))
      )
    )
  (helper (scdr s) (scar s) 0)
  )

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (stream-map (lambda (x) (* x 4)) (partial-sum (pi-summands 1))))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2 (partial-sum (ln2-summands 1)))
(define aln2 (euler-transform ln2))
(define saln2 (accelerated-sequence euler-transform ln2))

(define (run8)
  ; tests that partial sum works
  ; (stream-display (partial-sum (repeat 1)) 5) 
  ; tests that pi-stream works
  ; (stream-display pi-stream 8)
  ; ... which we need to test that the euler transform works
  ; (stream-display (euler-transform pi-stream) 5) 
  ; ... which we need to 
  (println "Approximation of ln(2) using the partial sum: ")
  (stream-display ln2 4)
  (println "Approximation of ln(2) using the partial sum, accelerated: ")
  (stream-display aln2 4)
  (println "Approximation of ln(2) using the partial sum, accelerated twice: ")
  (stream-display saln2 4)

  (println "ln2 and aln2 overflow before converging")
  (println "To what and when does the value of saln2 converge?")
  (println 
    "\t"
    (stream-ref saln2 (get-convergence saln2)) " at " (get-convergence saln2) 
    " iterations"
    )
  )

;==============================================================================
; Task 9
;==============================================================================

;==============================================================================
; Task 10
;==============================================================================

;==============================================================================
; Last Line!
;=============================================================================
(display "\n\nassignment 3 loaded!\n\n")
