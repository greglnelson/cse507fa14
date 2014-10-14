#lang racket

(require "graph.rkt" "problems.rkt" "solver.rkt" "k-coloring.rkt" rackunit)

; Extract all small easy problems (see problems.rkt):
(define small-problems
  (filter (lambda (p) 
          (eq? (problem-difficulty p) 'easy)
          (< (problem-nodes p) 30)
          (< (problem-edges p) 30))
        problems))

(define dsj
  (filter (lambda (p) 
          (eq? (problem-difficulty p) 'easy)
          (eq? (problem-nodes p) 500)
          (eq? (problem-edges p) 3555))
        problems))

; Print them (there is just one):
small-problems

(dot "/usr/local/bin/dot")

; Parse the problem into a graph representation (see graph.rkt):
(define small-graph
  (problem->graph (first small-problems)))

(define (run problem)
  (printf "---------~s---------\n" problem)
  (define graph (problem->graph problem))
  (define k (problem-colors problem))
  (define coloring (time (k-coloring graph k)))
  (printf "coloring: ~a\n" coloring)
  (printf "valid-coloring? ~a\n" (time (valid-coloring? graph coloring)))
  ;(visualize graph coloring)
  )

;(reverse-hash (make-hash-node-color-to-variable small-graph 1))
;(run (first small-problems))
(map (lambda (x) (run x)) problems)

(define (fold-stress times graph)
  (for ([t (in-range times)])
    (for/fold ([r (list )])
            ([e (edges graph)]
             [proxy-offset (for/list ([x (in-range (edge-count graph))]) x)])
    (append r (list e (* proxy-offset t))))))

(define (recurse-make times edges proxy-offset proxy-offset-max)
  (if (eq? proxy-offset proxy-offset-max)
      (list (* proxy-offset times))
      (cons (list (first edges) (* proxy-offset times)) (recurse-make times (rest edges) (+ proxy-offset 1) proxy-offset-max))))

(define (recurse-nand edges offset)
  (if (empty? edges)
      empty
      (cons (list (first edges)) (recurse-nand (rest edges) offset))))

; could return if too slow still
(define (recurse-stress times graph)
  (for ([t (in-range times)])
    (recurse-make t (for/list ([e (edges graph)]) e) 0 (edge-count graph))))


;(recurse-nand (stream->list (edges (problem->graph (first small-problems)))) 10)

#|
(time (fold-stress 1 (problem->graph (first dsj))))
(time (fold-stress 2 (problem->graph (first dsj))))
(time (fold-stress 4 (problem->graph (first dsj))))
(time (fold-stress 8 (problem->graph (first dsj))))

(time (recurse-stress 1 (problem->graph (first dsj))))
(time (recurse-stress 2 (problem->graph (first dsj))))
(time (recurse-stress 4 (problem->graph (first dsj))))
(time (recurse-stress 8 (problem->graph (first dsj))))
(time (recurse-stress 8000 (problem->graph (first dsj))))
|#
;(define gg (problem->graph (first dsj)))
;(recurse-make 10 (for/list ([e (edges gg)]) e) 0 (edge-count gg))

#|
(for ([k (in-range 1 105 1)])
(for ([n (in-range 1000)])
  (for ([c (in-range k)])
    (let ([o (find-offset 1 k)])
    (check-equal? n (deref-node (nk n c o) o))
    (check-equal? c (deref-color (nk n c o) o))
      ))))
|#

#|
(for ([k (in-range 97 108 1)])
(for ([n (in-range 100)])
  (for ([c (in-range k)])
    (let ([o (find-offset 1 k)])
    (check-equal? n (deref-node (nk n c o) o))
    (check-equal? c (deref-color (nk n c o) o))
      ))))
|#

;(quotient lit l)



; TODO if needed variable numbers for graph encoding are unique

; have right number of clauses in basic cnf
(define (cnf-test graph k)
  (define offset (find-offset 1 k))
  (define proxy-var-start (* 10 (to-lit (node-count graph) k offset)))
  (define cnf (make-cnf graph k offset proxy-var-start))
  (check-equal? (+ (node-count graph) (* k (edge-count graph)))
                (length cnf)) ; cnf right size
  ;(printf "cnf for k:~a ~a\n" k cnf)
  )

(for ([k (in-range 30)])
  (cnf-test small-graph k))

; silly "determinism" test that doesn't work
(check-equal? (make-hash-node-color-to-variable small-graph 3) (make-hash-node-color-to-variable small-graph 3) )

; all nodes in hash
(for ([k (in-range 30)])
       (let ([h (make-hash-node-color-to-variable small-graph k)])
         (for ([x (nodes small-graph)])
           (for ([c (in-range k)])
               (check-pred (curry hash-has-key? h) (list x c) )) )))

; cnf-node-has-at-least-one-color is uniq and length == k
#|
; old tests
(append (node-color-pairs 0 2) (node-color-pairs 1 2))
(all-node-color-pairs small-graph 2)

(define h (make-hash-node-color-to-variable small-graph 3))
(hash-has-key? (make-hash-node-color-to-variable small-graph 1) (list 0 0))

(reverse-hash (make-hash-node-color-to-variable small-graph 1))
(for ([x (nodes small-graph)])
  (print x))
(all-node-color-pairs  small-graph 1)

|#

;(check-eq? (make-hash-node-color-to-variable small-graph 1) X )
;(let ([pairs (all-node-color-pairs small-graph 1)])
;    (for/hash ([kk pairs]
;             [v (in-range 3)])
;    (values kk v)))
;(visualize small-graph)

;(visualize-degree small-graph)