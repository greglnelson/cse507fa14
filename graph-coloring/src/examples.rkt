
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

; The following vector represents a valid coloring for small-graph (see k-coloring.rkt):
(define small-graph-coloring #(0 1 3 1 0 0 1 3 3 0 2))
;(define small-graph-coloring #(1 2 3 4 5 6 7 8 9 10 11))

(valid-coloring? small-graph small-graph-coloring)

; If you have dot installed on your system, uncomment the following 
; lines to visualize small-graph:
(dot "/usr/local/bin/dot")
(visualize small-graph small-graph-coloring)

; Runs your k-coloring procedure (see k-coloring.rkt) on the provided problem, 
; printing timing data and #t/#f depending on whether the produced 
; coloring is valid or not. 
(define (run problem)
  (printf "---------~s---------\n" problem)
  (define graph (problem->graph problem))
  (define k (problem-colors problem))
  (define coloring (time (k-coloring graph k)))
  (printf "valid-coloring? ~a\n" (valid-coloring? graph coloring)))

; Uncomment to test your encoding of small-problem:
; (run small-problem)

; A SAT solver accepts a CNF formula, represented as a list of lists of 
; integers, and returns #f or an interpretation, depending on whether the 
; formula is satisfiable or not (see solver.rkt).
(solve '((1 -2) (-1) (2))) ; unsat
(solve '((-2) (1)))      ; sat

