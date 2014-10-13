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
;(visualize small-graph)

;(visualize-degree small-graph)