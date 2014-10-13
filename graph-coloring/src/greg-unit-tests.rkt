#lang racket

(require "graph.rkt" "problems.rkt" "solver.rkt" "k-coloring.rkt")

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

(visualize small-graph)

(visualize-degree small-graph)