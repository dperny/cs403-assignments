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

(define (range start stop step)
  (define (mod n m) (+ 1 (% (- n 1) m)))
  (define (iter store src)
    (cond 
      ((< src start) store)
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


(run1)
(run2)

(run5)

; Last statement!
(println "assignment 2 loaded")
