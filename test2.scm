(include "assign2.scm")

(println "Test Script: VERSION 1\n")

(println "author...\n")
(author)

(print "PROBLEM 1:")
(if (defined? 'run1 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run1)
        (println "\n-------my tests of your code--------\n")
        (inspect (range 0 10 1))
        (println "    [it should be (0 1 2 3 4 5 6 7 8 9)]")
        (for-loop (range 0 1 1) (lambda (i) (inspect i)))
        (println "    [it should be \"i is 0\"]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)


(print "PROBLEM 2:")
(if (defined? 'run2 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run2)
        (println "\n-------my tests of your code--------\n")
        (inspect ((pval + 2) 3))
        (println "    [it should be 5]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)
    
(print "PROBLEM 3:")
(if (defined? 'run3 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run3)
        (println "\n-------my tests of your code--------\n")
        (inspect (infix->prefix '(a + b)))
        (println "    [it should be (+ a b)]")
        (println)
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 4:")
(if (defined? 'run4 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run4)
        (println "\n-------my tests of your code--------\n")
        (inspect (let2locals '(define (f x) (let ((y a)) (+ x y)))))
        (println "    [it should be (define (f x) (define y a) (+ x y))]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 5:")
(if (defined? 'run5 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run5)
        (println "\n-------my tests of your code--------\n")
        (define one (lambda (i) (lambda (b) (i b))))
        (inspect (((cmult one one) (lambda (x) (+ x 1))) 0))
        (println "    [it should be 1]")
        (inspect (((cpow one one) (lambda (x) (+ x 1))) 0))
        (println "    [it should be 1]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 6:")
(if (defined? 'run6 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run6)
        (println "\n-------my tests of your code--------\n")
        (define (nilify x) (map (lambda (i) (if (nil? i) 'nil i)) x))
        (inspect (nilify (powerSet '(1))))
        (println "    [it should be (nil (1))]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 7:")
(if (defined? 'run5 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run5)
        (println "\n-------my tests of your code--------\n")
        (inspect (dot-product '(1 2) '(3 4)))
        (println "    [it should be 11]")
        (inspect (matrix-*-vector '((1 2) (3 4)) '(5 6)))
        (println "    [it should be (17 39)]")
        (inspect (transpose '((1 2) (3 4))))
        (println "    [it should be ((1 3) (2 4))]")
        (inspect (matrix-*-matrix '((1 2) (3 4)) '((1 0) (0 1))))
        (println "    [it should be ((1 2) (3 4))]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 8:")
(if (defined? 'run8 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run8)
        (println "\n-------my tests of your code--------\n")
        (inspect (extract 'h '(1 2 3 4 5 6)))
        (println "    [it should be 1]")
        )
    (println " NOT IMPLEMENTED\n")
    )
(println)

(print "PROBLEM 9:")
(if (defined? 'run9 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run9)
        (println "\n-------my tests of your code--------\n")
        (inspect (catch (- "love" "hate")))
        (println "    [it should be an error]")
        (define check (time))
        (inspect (overloadMinus "words10000"))
        (println "    [that took " (- (time) check) " seconds]")
        (define check (time))
        (inspect (member? "facade" (diff "love" "hate")))
        (println "    [that took " (- (time) check) " seconds]")
        (println "    [it should be #t]")
        (inspect (- 5 3))
        (println "    [it should be 2]")
        (inspect (underloadMinus))
        (inspect (- 54 34))
        (println "    [it should be 20]")
        (inspect (catch (- "love" "hate")))
        (println "    [it should be an error]")
        )
    (println " NOT IMPLEMENTED\n")
    )
(println)

(print "PROBLEM 10:")
(if (defined? 'run10 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run10)
        (println "\n-------my tests of your code--------\n")
        (inspect (catch (+ (rectangular 2 3) (polar 2 3))))
        (println "    [it should be an error]")
        (inspect (overloadPlus))
        (inspect (+ (polar 0 0) (polar 0 0)))
        (println "    [it should be polar]")
        (inspect (+ (rectangular 0 0) (rectangular 0 0)))
        (println "    [it should be rectangular]")
        (inspect (underloadPlus))
        (inspect (+ 54 34))
        (println "    [it should be 88]")
        (inspect (catch (+ (rectangular 2 3) (polar 2 3))))
        (println "    [it should be an error]")
        )
    (println " NOT IMPLEMENTED\n")
    )
(println)

(author)