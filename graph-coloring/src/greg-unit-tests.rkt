#lang racket

(require "graph.rkt" "problems.rkt" "solver.rkt" "k-coloring.rkt" rackunit)

; Extract all small easy problems (see problems.rkt):
(define small-problems
  (filter (lambda (p) 
          (eq? (problem-difficulty p) 'easy)
          (< (problem-nodes p) 30)
          (< (problem-edges p) 30))
        problems))

; Print them (there is just one):
small-problems

; Parse the problem into a graph representation (see graph.rkt):
(define small-graph
  (problem->graph (first small-problems)))

; variable numbers for graph encoding are unique
;(check-pred 

(append (node-color-pairs 0 2) (node-color-pairs 1 2))
(all-node-color-pairs small-graph 2)

(make-hash-node-color-to-variable small-graph 1) 
(hash-has-key? (make-hash-node-color-to-variable small-graph 1) (list 0 0))

; all nodes in hash
(for ([k (in-range 30)])
       (let ([h (make-hash-node-color-to-variable small-graph k)])
         (for ([x (nodes small-graph)])
           (for ([c (in-range k)])
               (check-pred (curry hash-has-key? h) (list x c) )) )))
(node-count small-graph)
(for ([x (nodes small-graph)])
  (print x))
(all-node-color-pairs  small-graph 1)
;(check-eq? (make-hash-node-color-to-variable small-graph 1) X )
;(let ([pairs (all-node-color-pairs small-graph 1)])
;    (for/hash ([kk pairs]
;             [v (in-range 3)])
;    (values kk v)))
;(visualize small-graph)

;(visualize-degree small-graph)