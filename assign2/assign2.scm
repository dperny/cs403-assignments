;
; CS 403
; Assignment 2
; Drew Erny
;

; author function required
(define (author)
  (println "AUTHOR: Drew Erny dperny@crimson.ua.edu")
  )

; tests expressions for exceptions, then displays
; expected and recieved outcomes
; params: 
;   $expr - the expression to test
;   target - the expected output
(define (exprTest # $expr target)
  (define result (catch (eval $expr #)))
  (if (error? result)
    (println $expr " is EXCEPTION: " (result'value)
      " (it should be " target " )")
    (println $expr " is " result
      " (it should be " target ")")
    )
  )

;
; Task 1
;

;{
; IGNORE ME.
; This code, while cool, is utter broken for most edge cases
(define (range start stop step)
  (define (mod n m) (+ 1 (% (- n 1) m)))
  (define (iter store src)
    (cond 
      ((and (< 0 step) (< src start)) store)
      ((and (> 0 step) (> src start)) store)
      (else
        (iter 
          (cons src store)
          (- src step)
          )
        )
      )
    )
  (iter nil (- stop (mod (- stop start) step)))
  )
;}

; this code works, but is recursive
(define (range start stop step)
  (define (help c)
    (cond
      ((or
        (and (> step 0) (< c stop))
        (and (< step 0) (> c stop)))
          (cons c (help (+ step c)))
        )
      (else nil)
      )
    )
  (help start)
  )

(define (for-loop arglist procedure)
  (cond
    ((null? arglist) nil)
    (else (begin
      (procedure (car arglist))
      (for-loop (cdr arglist) procedure)
      ))
    )
  )

(define (run1)
  (inspect (range 10 1 -1))
  (inspect (range 1 10 1))
  (inspect (range 0 0 1))
  (for-loop (range 0 5 1) (lambda (i) (print i ", ")))
  (println)
  (for-loop (range 1 13 4) (lambda (j) (print j ", ")))
  (println)
  (for-loop (range 1 15 4) (lambda (k) (print k ", ")))
  (println)
  )
;
; Task 2
;
(define (pval @)
  (let ((outer @))
    (lambda (@)(apply (car outer) (append (cdr outer) @)))
    )
  )

(define (run2)
  ; all these should be the same
  (println "the next five lines should be the same:")
  (inspect(+ 1 2 3))
  (exprTest ((pval +) 1 2 3) 6)
  (exprTest ((pval + 1) 2 3) 6)
  (exprTest ((pval + 1 2) 3) 6)
  (exprTest ((pval + 1 2 3)) 6)
  )

;
; Task 3
;


;
; Task 5
;

(define (cadd a b)
  (lambda (f)
    (lambda (x)
      ; use the result of b as the base for a
      ((a f)((b f) x))
      )
    )
  )

(define (cmult a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x)
      )
    )
  )

(define (cpow a b)
  (b a) ; that is way to easy to take me as long as it did to figure out
  )

(define (run5)

  (define (church2dec num) 
    ((num (lambda (x) (+ x 1))) 0)
    )

  (define one (lambda (f) (lambda (x) (f x))))
  (define two (cadd one one))
  (define three (cadd two one))
  
  (exprTest (church2dec (cmult three two)) 6)

  (exprTest (church2dec (cpow three two)) 9)
  )

;
; Task 7
;

(define (matrix-*-vector m v)
  )

(define (transpose m)
  )

;
; Task 8
;

; Given a symbol set of instructions in the format
;   'htththhththt (where h means take the head, and t means take the tail)
; And some list or nested lists
;
; Returns the result of the instructions applied to the target
(define (extract instructions target)
  (define (distill str li)
    (cond 
      ((null? str) li)
      ((equal? "h" (car str))             ; h = take the car target
        (distill (cdr str) (car li))) ; and advance instruction string
      ((equal? "t" (car str))
        (distill (cdr str) (cdr li))) ; t = take the cdr of target
      )
    )
  (distill (string instructions) target)
  )

(define (run8)
  (exprTest (extract 't '(1 2 3 4)) '(2 3 4))
  (exprTest (extract 'th '(1 2 3 4)) 2)
  (exprTest (extract 'ht '((1) 2 3 4)) nil)
  )

(run1)
(run2)

(run5)

(run8)

; Last statement!
(println "assignment 2 loaded")
