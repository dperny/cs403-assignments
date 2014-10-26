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
  (inspect (+ 1 2 3))
  (exprTest ((pval +) 1 2 3) 6)
  (exprTest ((pval + 1) 2 3) 6)
  (exprTest ((pval + 1 2) 3) 6)
  (exprTest ((pval + 1 2 3)) 6)
  )

;
; Task 3
;

(define (infix->prefix expression)
  (define (rank sym)
    (cond 
      ((or (eq? sym '+) (eq? sym '-)) 1)
      ((or (eq? sym '*) (eq? sym '/)) 2)
      ((eq? sym '^) 3)
      (else 0)
      )
    )

  )

(define (norun3)
  (exprTest
    (infix->prefix '(2 + 3 * x ^ 5 + a))
    '(+ 2 (+ (* 3 (^ x 5)) a))
    )

  )

;
; Task 4
;

(define (let2locals function)
  (if (equal? (car (caddr function)) 'let)
    ; keep the original define
    (append
      (cons (car function)
        ; keep the original parameters list
        (cons (cadr function)
          (cons 
            (map (lambda (x) (cons 'define x)) (cadr (caddr function)))
            nil
            )
          )
        )
      (cddr (caddr function))
      )
    )
  )

(define (let2locals function)
  (if (equal? (car (caddr function)) 'let)
    (append
      (append
        (list (car function) (cadr function))
        (map (lambda (x) (cons 'define x)) (cadr (caddr function)))
        )
      (cddr (caddr function))
      )
    function
    )
  )

(define (run4)
  (inspect 
    (let2locals
      '(define (f x) (let ((y 1) (z 2)) (* x y z)))
      )
    )

  (inspect
    (let2locals 
      '(define (f x) (define y 1) (define z 2) (* x y z))
      )
    )
  )

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
; Task 6
;

(define (F e T)
  (map
    (lambda (X)
      (cons e X)
      )
    T
    )
  )

(define (powerSet S)
  (cond
    ((null? S) '(()) )
    (else 
      (let ( (T (powerSet (cdr S))) (e (car S)) )
        (merge
          T
          (F e T)
          )
        )
      )
    )
  )

(define (merge a b)
  (define (help f s)
    (cond 
      ((null? f) s)
      ((null? s) f)
      (else
        (cons (car f) (cons (car s) (help (cdr f) (cdr s))))
        )
      )
    )
  (help a b)
  )

(define (run6)
  ;(for-loop (powerSet '(a b c)) (lambda (x) (println x)))
  (inspect (powerSet '(a b c)))
  )

;
; Task 7
;

; thank you, oh glorious INTERNET, for your assistance with these problems

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence))
        )
    )
  )

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
    nil
    (cons (accumulate op initial (map car sequences))
          (accumulate-n op initial (map cdr sequences))
          )
    )
  )

(define (dot-product v w)
  (accumulate + 0 (map * v w))
  )

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m)
  )

(define (transpose m)
  (accumulate-n cons nil m)
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)
    )
  )

(define (run7)
  (exprTest (dot-product '(1 2 3) '(3 2 1)) 10)
  (exprTest (matrix-*-vector '((1 2 3) (1 2 3) (1 2 3)) '(3 2 1)) '(10 10 10))
  (exprTest (transpose '((0 1) (2 0))) '((0 2) (1 0)))
  (exprTest
    (matrix-*-matrix
      '((1 2 3) (4 5 6) (7 8 9))
      '((9 8 7) (6 5 4) (3 2 1))
      )
    '((30 24 18) (84 69 54) (138 114 90))
    )
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

;
; Task 9
;

; save old minus
(define minus -)

(define wordlist)

(define (findMatch n)
  (define (iter store src)
    (if (null? src)
      store
      (iter
        (if (= (cdr (car src)) n)
          (cons (car (car src)) store)
          store
          )
        (cdr src)
        )
      )
    )
  (iter nil wordlist)
  )

(define (diff x y)
  (let ((a (word2int x)) (b (word2int y)))
    (cond
      ((= (- a b) 0) "they are the same")
      (else 
        (let ((matchlist (findMatch (abs (- a b)))))
          (if (null? matchlist)
            "the difference is unknown to us"
            matchlist
            )
          )
        )
      )
    )
  )

; allows for the subtraction of strings
(define (minusPlus @)
  (if (and (equal? (type (car @)) 'STRING) (equal? (type (cadr @)) 'STRING))
    (diff (car @) (cadr @))
    (apply minus @)
    )
  )

; converts a word to its gemetria representation
(define (word2int word)
  (accumulate + 0 (map (lambda (x) (minus (ascii x) 96)) word))
  )

(define (readWords filename)
  (define p (open filename 'read))
  (define oldPort (setPort p))
  (define (iter words)
    (if (not (eof?))
      (let ((word (string (readToken))))
        (iter (cons (cons word (word2int word)) words))
        )
      words
      )
    )
  (iter nil)
  )

(define (overloadMinus filename)
  (set! wordlist (readWords filename))
  (set! - minusPlus)
  )

(define (underloadMinus)
  (set! - minus)
  )


(define (run9)
  (overloadMinus "words10000")
  
  ; test that old minus still works
  (exprTest (- 3 2 1) 0)

  ; test that we can turn words into their integer representation
  (exprTest (word2int "love") 54)
  (exprTest (word2int "hate") 34)
  
  ; a word that describes itself is said to be autological
  ; is autological autological?
  (inspect  (- "autological" "autological" ))
  ; gemetria says they are the same

  ; lol how silly
  (inspect (- "lusth" "heresy"))

  (inspect (member? "deficits" (- "programming" "lisp")))
  (inspect (member? "slain" (- "windows" "gates")))

  (inspect (- "somemassivestringnowaytheresoneofthese" "lol"))

  (underloadMinus)
  )

;
; Task 10
;

(define pi 3.14159265358979323846264338327950) ; all the digits i know off hand

; just gonna leave this as a monument to my stupidity
; READ THE DOCS
;{
(define (atan2 y x)
  (cond
    ((> x 0) (atan (/ y x)))
    ((and (>= y 0) (< x 0)) (+ (atan (/ y x)) pi))
    ((and (< y 0) (< x 0)) (- (atan (/ y x)) pi))
    ((and (> y 0) (= x 0)) (/ pi 2))
    ((and (< y 0) (= x 0)) (- 0 (/ pi 2)))
    ((and (= x 0) (= x 0)) 0)
    )
  )
;}

(define plus +)

(define (polar r theta)
  (list 'polar (real r) (real theta))
  )

(define (rectangular x y)
  (list 'rectangular (real x) (real y))
  )

(define (overloadPlus)
  (set! + extAddext)
  )

(define (underloadPlus)
  (set! + plus)
  )

; convert a complex number in polar form to rectangular form
(define (polar2rect c)
  (rectangular
    (* (cadr c) (cos (caddr c)))
    (* (cadr c) (sin (caddr c)))
    )
  )

; convert a complex number in rectangular form to polar form
(define (rect2polar c)
  (polar
    (sqrt (+ (^ (cadr c) 2) (^ (caddr c) 2)))
    (atan (caddr c) (cadr c))
    )
  )

; adds two complex numbers in rectangular
; coordinate form
(define (addRect a b)
  (rectangular
    (plus (cadr a) (cadr b))
    (plus (caddr a) (caddr b))
    )
  )

; adds a polar number to another polar number
(define (addPolar a b)
  (rect2polar
    (addRect
      (polar2rect a)
      (polar2rect b)
      )
    )
  )

; takes any number, complex or not, and turns it into a rectangular number
(define (toRect num)
  (if (equal? (type num) 'CONS)   ; if num is already complex
    (if (equal? (car num) 'polar) ;   but it is polar
      (polar2rect num)            ;     convert to rectangular
      num                         ;     otherwise, it's already rectangular
      )
    (rectangular num 0)           ;   otherwise, cast to rectangular (simple) 
    )
  )

; takes any number, complex or not, and turns it into a polar number
(define (toPolar num)
  (if (equal? (type num) 'CONS)   ; if num is already complex
    (if (equal? (car num) 'polar) ;   and it is polar
      num                         ;     return as is
      (rect2polar num)            ;     otherwise, conver to polar
      )
    (polar num 0)           ;   otherwise, cast to polar (simple) 
    )
  )

; allows for the addition of complex numbers in rectangular and polar form.
(define (extAdd a b)
  (cond                 ; check for complex numbers

    ((and (equal? (type a) 'CONS) (equal? (car a) 'rectangular)) ; a is rect
      (addRect a (toRect b))                                     
      )

    ((and (equal? (type a) 'CONS) (equal? (car a) 'polar))       ; a is polar
      (addPolar a (toPolar b))
      )

    ((and (equal? (type b) 'CONS) (equal? (car b) 'rectangular)) ; b is rect
      (addRect b (toRect a))
      )

    ((and (equal? (type b) 'CONS) (equal? (car b) 'polar))       ; b is polar
      (addPolar b (toPolar a))
      )

    (else (plus a b)) ; no complex numbers? pass to old plus
    )
  )

; lets you chain adds
(define (extAddext a b @)
  (cond 
    ((null? @) (extAdd a b))
    (else
      (extAdd a (apply extAddext (cons b @)))
      )
    )
  )

(define (run10)
  ; canned complex numbers for testing
  (define polUnit (polar (sqrt 2) (/ pi 4)))
  (define rectUnit (rectangular 1 1))

  ; overload start
  (overloadPlus)

  ; make sure old plus is still around
  (exprTest (plus 1 2 3) 6)

  ; make sure it still works the way it used to
  (exprTest (+ 1 2) 3)
  (exprTest (+ 1 2 3) 6)
  
  ; test adding rect to real
  (exprTest (+ (rectangular 1 1) 1) (rectangular 2 1))
  (exprTest (+ 1.0 (rectangular 1 1)) (rectangular 2 1))
  
  ; test adding pol to real
  (exprTest (+ polUnit 1) (rect2polar (rectangular 2 1)))
  (exprTest (+ 1 polUnit) (rect2polar (rectangular 2 1)))

  ; test adding pol to pol and rect to rect
  (exprTest (+ polUnit polUnit) (polar (* 2 (sqrt 2)) (/ pi 4)))
  (exprTest (+ rectUnit rectUnit) (rectangular 2 2))

  ; test adding rect to pol and pol to rect
  (exprTest (+ rectUnit polUnit) (rectangular 2 2))
  (exprTest (+ polUnit rectUnit) (polar (* 2 (sqrt 2)) (/ pi 4)))

  ; test adding a bunch of stuff
  (exprTest (+ rectUnit polUnit rectUnit) (rectangular 3 3))

  ; underload plus. winrar
  (underloadPlus)
  )

; Last statement!
(println "assignment 2 loaded")
