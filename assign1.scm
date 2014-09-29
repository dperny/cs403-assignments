;
; CS 403
; Assignment 1
; Drew Erny
;

; define author
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
; Assignment 1, Problem 1
; 

; my-cond as described in the problem 1 description
(define (my-cond @)
  (define (iter items)
    (if (null? items)
      nil
      ; else
      (if (car items)
        (cadr items)
        (iter (cddr items))
        ) ; /car items
      ) ; /null?
    ) ; /iter
  (iter @)
  )

(define (test1)
  (define x 2)
  (define y 4)

  ;(inspect
    (cond
      ((= x y) 0)
      ((> x y) 1)
      ((< x y) -1)
      )
  ;  )

  ;(inspect
    (my-cond
      (= x y) 0
      (> x y) 1
      (< x y) -1
      )
   ; )

  (println
"cond short circuits its evaluation. it only evaluates the result 
of the first true condition.

mycond evalutes all of the results as they pass, until one of 
the conditions is true. it then returns the result of the true
condition.
")
  (inspect
    (cond
      (#f (println "this doesn't evaluate"))
      (#t (println "cond evalutes only blocks where the condition is true"))
      )
    )

  ; print a linebreak for to make this more pretty
  (println "")

  (inspect
    (my-cond
      #f (println "my-cond evaluates")
      #t (println "all blocks")
      )
    )
  )

; run problem 1
(define (run1)
  (test1)
  )

;
; Assignment 1, Problem 2
;

(define (min8 p1 p2 p3 p4 p5 p6 p7 p8)

  ; compares two numbers, returns the lesser of the two
  (define (min2 x y)
    (if (< x y) x y)
    )

  (min2 ; 1
    (min2 ; 2
      (min2 p1 p2) ; 3
      (min2 p3 p4) ; 4
      )
    (min2 ; 5
      (min2 p5 p6) ; 6
      (min2 p7 p8) ; 7
      )
    )
  )

(define (run2)
  ; a variety of simple test
  (exprTest (min8 1 2 3 4 5 6 7 8) 1)
  (exprTest (min8 8 7 6 5 4 3 2 1) 1)
  (exprTest (min8 7 8 2 1 3 6 4 5) 1)
  (exprTest (min8 5 4 6 3 7 2 8 1) 1)
  )

; Assignment 1, Problem 3

; takes a threshold as a single argument,
; then returns a function to tell if that argument diverges
(define (mandelbrot-iter threshold)
  ; takes point (x, y) and checks if it
  ; is within the mandlebrot set with threshold
  ; of certainty
  (lambda (x y)
    ; recursive function to check divergence
    (define (iter r s t)
      ; check for threshold reached
      (if (= t threshold) 
        0
        ; check for divergence
        (if (< 2 (sqrt (+ (* r r) (* s s))))
          ; if diverges, return t (the number of iterations checked)
          t
          ; otherwise, iterate one level deeper
          (iter 
            ; r value
            (+ x (- (* r r) (* s s)))
            ; s value
            (+ y (* r s 2))
            ; t value
            (+ 1 t)
            )
          )
        )
      )
    ; call with default values 0.0 for r and 0.0 for s
    (iter 0.0 0.0 0)
    )
  )

; (0, 0) should yeild 0
(define (run3)
  (define mandelbrot-tester (mandelbrot-iter 100))
  (exprTest (mandelbrot-tester 0 0) 0)
  (inspect (mandelbrot-tester -1.5 0.0001))
  (inspect (mandelbrot-tester 14 14))
  )

;
; Assignment 1, Task 4
;

; given a number, x, returns an approximation of its cube root by using
; a binary search
(define (root3 x)
  ; iterator helper
  (define (search low high)
    ; base case, difference between high and low less than/equal to 0.0001
    ; (this is the precision of the function)
    (if (<= (- high low) .0001)
      ; return low
      low
      ; else, check which side of the mid we should recur to
      ; if mid cubed is greater than x
      (if (<= x (^ (/ (+ low high) 2) 3))

        ; search for a smaller mid (low to mid recurrence)
        (search low (/ (+ low high) 2))
        
        ; otherwise, search for a greater mid (mid to high recurrence)
        (search (/ (+ low high) 2) high)
        )
      )
    )
  (search (real 0) (real x))
  )

(define (run4)
  (exprTest (root3 8) 2)
  (exprTest (root3 (^ 22 3)) 22)
  )

;
; Assignment 1, Task 5
;

; given a number of levels, prints a modified pascal's triangle where the left
; and right values are left and right
(define (crazyTriangle left right levels)
  ; helper to find the value of the cell we want to populate 
  (define (helper level column)
    (cond
      ((= column 0) left)      ; if we're on column 0, return left
      ((= level column) right) ; if col == level, then return right
      (else                    ; otherwise, recur
        (+ 
          (helper (- level 1) (- column 1)) ; up and to the right
          (helper (- level 1) column)       ; directly above
          )
        )
      )
    )

  ; adds spaces
  (define (pad spaces)
    (if (> spaces 0)       ; if there are spaces to pad
      (begin
        (print " ")        ; add a spaces
        (pad (- spaces 1)) ; then add more spaces 
        )
      )
    )

  (define (printlvl level)
    (if (<= level levels)                       ; while level <= levels
      (begin 
      (pad (- levels level))                    ; pad some spaces
        (define (printcol col)
          (if (<= col level)                    ; while col <= level
            (begin 
              (print (helper level col) " ")    ; print the cell and a space
              (printcol (+ col 1))              ; print next cell in col
              )
            )
          )
          (printcol 0)           ; print from column 0 on this line
          (println)              ; newline between levels
          (printlvl (+ level 1)) ; print the next level
        )
      )
    )

  (printlvl 0)
  )


(define (run5)
  (inspect (crazyTriangle 0 0 0))
  (inspect (crazyTriangle 1 1 1))
  (inspect (crazyTriangle 1 1 5))
  (inspect (crazyTriangle 2 2 4))
  )
;
; Assignment 1, Task 6
;

; function which takes a function and
; the number of arguments to start to apply to it.
(define (pfa f k)
  (cond
    ((= k 1) 
      (lambda (x) ; new function, taking 1 argument
        (lambda (@) ; returns new function taking some number of args
          (apply f (cons x @)) ; and applies f to that arg list.
          )
        ))
    ; same as above, but with more args
    ((= k 2) 
     (lambda (x y) (lambda (@) (apply f (cons x (cons y @))))))
    ((= k 3) 
     (lambda (x y z) (lambda(@) (apply f (cons x (cons y (cons z @)))))))
    )
  )

(define (run6)
  (define a 1)
  (define b 2)
  (define c 3)
  (define (f x y z) (+ x (* y z)))
  (exprTest (((pfa f 1) a) b c) (f a b c))
  (exprTest (((pfa f 2) a b) c) (f a b c))
  (inspect (((pfa println 3) "first arg, " "second arg, " "third arg, ") 
            "some " "more " "args "))
  )

; 
; Assignment 1, Task 7
;

; This is recursive Zarp. This is not a solution, but because
; it's exactly defined like this in the problem guide, it can be used
; to check solutions of the iterative version.
(define (zarpr i)
  (if (< i 3)
    i
    (-
      (+
        (zarpr (- i 1))
        (* 2 (zarpr (- i 2)))
        )
      (zarpr (- i 3))
      )
    )
  )

; iterative zarp. given an integer i, computes zarp(i) as described in
; Assignment 1 on 9/13/14
(define (zarp i)
  ; if i < 3 just return i
  (if (< i 3) i
    ; otherwise, for 3 or more, we need to zarp UP to i
    (begin
      ; first, define an iterator function
      (define (iter n prev1 prev2 prev3)
        ; if we've not yet found zarp(i)
        (if (<= n i)
          (iter                             ; find zarp(n+1), and:
            (+ 1 n)                         ; increment the counter
            (- (+ prev1 (* 2 prev2)) prev3) ; calculate zarp(n)
            prev1                           ; zarp(n-1) was previously known
            prev2                           ; zarp(n-2) was also known
            ; prev3                         ; zarp(n-3) no longer needed
            )
          ; otherwise, when we're past the number of zarps we need to count
          ; up to, (so i + 1), just return the last zarp we calucated
          prev1
          )
        )
      ; we know that for 3, zarp(i-1) = 2, zarp(i-2) = 1, and zarp(i-3) = 0
      ; so call iter starting at 3 with these as prev1, prev2, and prev3
      (iter 3 2 1 0)
      ) ; end else block
    ) ; end if statement
  ) ; end (zarp i)

(define (run7)
  ; check a bunch of iterative zarps off the recursive version
  (exprTest (zarp 0) (zarpr 0))
  (exprTest (zarp 1) (zarpr 1))
  (exprTest (zarp 2) (zarpr 2))
  (exprTest (zarp 3) (zarpr 3))
  (exprTest (zarp 4) (zarpr 4))
  (exprTest (zarp 5) (zarpr 5))
  (exprTest (zarp 6) (zarpr 6))
  )

;
; Assignment 1, Task 8
;

; returns a number that is twice n
(define (double n) (+ n n))

; returns a number that is one half n
(define (halve n)
  (define (iter subs total)
    (if (>= total 2)
      (iter (+ subs 1) (- total 2))
      subs
      )
    )
  (iter 0 n) 
  )

; returns #t if n is even, #f otherwise. works only for positive integers
(define (even? n) (= (halve n) (halve (+ n 1))))

(define (ethi* x y)
  (define (iter total a b)  ; define the iterator function
    (if (>= b 1)              ; if b isn't 1
      (iter                   ; iterate to the next step and:
        (+ total              ;   add a to our total if b isn't even
          (if (even? b) 0 a)  ;   otherwise, just add 0
          )                   ;
        (double a)            ;   double a
        (halve b)             ;   halve b
        )
      total ; if b < 1, return the total we've accrued. this is the answer
      )
    )
    ; call the base iterator with no total, x and y as starting a and b
    (iter 0 x y)
  )

(define (run8)
  (exprTest (halve 28) 14)
  (exprTest (halve 6) 3)
  (exprTest (halve 7) 3)
  (exprTest (even? 6) #t)
  (exprTest (even? 7) #f)
  (exprTest (ethi* 1960 56) 109760)
  (exprTest (ethi* 420 69) (* 420 69))
  )

;
; Assignment 1, Task 9
; 

; returns an approximation of the trancendental number e using its continued
; fraction representation to an accuracy of n steps.
(define (ecf n)
  (define (helper depth)                ; h(depth) =
    (if (<= depth n)                    ; (if depth <= the depth we want)
      (/ 1                              ;               1
        (+ 1                            ; ---------------------------------
          (/ 1                          ; 1 +              1
            (+ (* 2 depth)              ;     -----------------------------
              (/ 1                      ;     (depth * 2) +        1
                (+ 1                    ;                  ----------------
                  (helper (+ depth 1))  ;                  1 + h(depth + 1)
                  )
                )
              )
            )
          )
        )
      0                                 ; or 0 if we've hit the max depth needed
      )
    )
  (+ 2 (helper 1.0))
  )

(define (run9)
  (println "  ecf is a recursive function")
  (exprTest (ecf 0) 2)
  (exprTest (ecf 1) 2.75)
  (exprTest (ecf 2) 2.717948717948718)
  (println)
  (inspect (ecf 100))
  (print "The actual value of e is approx. 2.718281828459045")
  )

; 
; Assignment 1, Task 10
;

; given a depth, calculates the given Ramanujan series to that depth of
; approximation using a recursive process
(define (ramanujan depth)
  (define (h x) 
    (if (<= (- x 1) depth)
      (* x                         ; x times
        (sqrt                     ; the square root of 
          (+ (+ x 5)              ; (x+5) plus
            (h (+ x 1))           ; the recursion, with (x+1)
            )
          )
        )
      0                            ; add zero when we hit max depth
      )
    )
  (h 1)
  )

; given a depth, calculates the given Ramanujan series to that depth of
; approximation using an iterative process
(define (iramanujan depth)
  (define (iter store src)                 ; define an iterator function
    (if (> src 0)                          ; we're working from the depth, up
      (iter                                ; iterate, and: 
        (* src (sqrt (+ (+ src 5) store))) ;   calculate new store w/ old store
        (- src 1)                          ;   decrease the depth by 1
        )
      store                                ; once we hit min depth, return store
      )
    )
  (iter 0 (+ 1 depth)) ; call iterator with (depth + 1)
  )                    ; (because depth of 0 doesn't make sense to the maths)

(define (run10)
  (exprTest (ramanujan 0) (sqrt 6))
  (exprTest (ramanujan 1) (sqrt (+ 6 (* 2 (sqrt 7)))))
  (exprTest (ramanujan 2) (sqrt (+ 6 (* 2 (sqrt (+ 7 (* 3 (sqrt 8))))))))
  (println)
  (exprTest (iramanujan 0) (sqrt 6))
  (exprTest (iramanujan 1) (sqrt (+ 6 (* 2 (sqrt 7)))))
  (exprTest (iramanujan 2) (sqrt (+ 6 (* 2 (sqrt (+ 7 (* 3 (sqrt 8))))))))
  (println)
  (println "this function converges to " (iramanujan 1000))
  )


; Last statement!
(println "assignment 1 loaded")


