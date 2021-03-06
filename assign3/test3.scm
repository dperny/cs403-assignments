(include "pretty.lib")
(include "assign3.scm")

(println "Test Script: VERSION 3\n")

(define (scadr s) (stream-car (stream-cdr s)))
(define (scaddr s) (stream-car (stream-cdr (stream-cdr s))))
(define (scadddr s) (stream-car (stream-cdr (stream-cdr (stream-cdr s)))))

(define (fakestream items)
    (cons-stream
        (car items)
        (fakestream (cdr items))
        )
    )

(println "author...\n")

(author)

(println)

(print "PROBLEM 1:")
(if (defined? 'run1 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run1)
        (println "\n-------my tests---------------------\n")
        (inspect (define (a x) this))
        (println)
        (inspect (define o (a 5)))
        (println)
        (inspect (set! (o'x) 6))
        (println)
        (inspect (o'x))
        (println "    [it should be 6]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 2:")
(if (defined? 'run2 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run2)
        (println "\n-------my tests---------------------\n")
        (inspect (define (multiply a b) (* a b)))
        (println)
        (inspect (replace multiply '* *))
        (println)
        (println "multiply now is:")
        (pretty multiply)
        (println)
        (inspect (multiply 10 3))
        (println "    [it should be 30]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 3:")
(if (defined? 'run3 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run3)
        (println "\n-------my tests---------------------\n")
        (inspect (define p (PRQ <)))
        (println)
        (inspect (p'empty?))
        (println "    [it should be #t]\n")
        (inspect (p'size))
        (println "    [it should be 0]\n")
        (inspect (p'insert 0 0))
        (println)
        (inspect (p'item))
        (println "    [it should be 0]\n")
        (inspect (p'rank))
        (println "    [it should be 0]\n")
        (inspect (p'remove))
        (println "    [it should be 0]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 4:")
(if (defined? 'run4 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run4)
        (println "\n-------my tests---------------------\n")
        (inspect (define v (connector)))
        (inspect (define g (connector)))
        (inspect (define h (connector)))
        (println)
        (inspect (speed v g h))
        (println)
        (inspect (set-value! v 0 'me))
        (println)
        (inspect (get-value v))
        (println "    [should be 0]\n")
        (inspect (forget-value! v 'me))
        (println)
        (inspect (set-value! v 1 'you))
        (println)
        (inspect (get-value v))
        (println "    [should be 1]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 5:")
(if (defined? 'run5 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run5)
        (println "\n-------my tests---------------------\n")
        (define (f) ((m'p)) (println "f") ((m'v)))
        (define (g) ((m'p)) (println "g") ((m'v)))
        (define (h) ((m'p)) (println "h") ((m'v)))
        (println)
        (inspect (define m (mmutex 100)))
        (println)
        (pexecute (f) (g) (h))
        (println)
        )
    (println " NOT IMPLEMENTED (extra credit)\n")
    )

(print "PROBLEM 6:")
(if (defined? 'run6 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run6)
        (println "\n-------my tests---------------------\n")
        (inspect (stream-car big-gulp))
        (println "    [it should be 7]\n")
        (inspect (stream-car (stream-cdr big-gulp)))
        (println "    [it should be 11]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 7:")
(if (defined? 'run7 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run7)
        (println "\n-------my tests---------------------\n")
        (inspect (stream-car twinPrimes))
        (println "    [it should be (3 5)]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 8:")
(if (defined? 'run8 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run8)
        (println "\n-------my tests---------------------\n")
        (println "first element of ln2: " (stream-car ln2))
        (println "    [it should be 1.000000]\n")
        (println "first element of aln2: " (stream-car aln2))
        (println "    [it should be 0.700000]\n")
        (println "first element of saln2 " (stream-car saln2))
        (println "    [it should be 1.000000]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 9:")
(if (defined? 'run9 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run9)
        (println "\n-------my tests---------------------\n")
        (define nats (fakestream '(1 2 3 4 5)))
        (inspect (stream-car (pairs nats nats)))
        (println "    [it should be (1 1)]\n")
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )

(print "PROBLEM 10:")
(if (defined? 'run10 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run10)
        (println "\n-------my tests---------------------\n")
        (inspect (node 10 20 nil))
        (inspect
            (do bind-pq
                (list (lambda (x) (+ x 10)) 100 (node nil nil nil)) 
                (pq-insert 20)
                (set-time 55)
                (set-tick (lambda (y) (+ y 50)))
                )
            )
        (println)
        )
    (println " NOT IMPLEMENTED\n")
    )
