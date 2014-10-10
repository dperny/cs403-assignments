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
  (define (iter store src)
    ()
    )
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

(run2)

; Last statement!
(println "assignment 2 loaded")
