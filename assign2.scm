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

(run1)
(run2)

; Last statement!
(println "assignment 2 loaded")
