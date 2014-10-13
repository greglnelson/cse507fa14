#lang racket

(require "solver.rkt" "graph.rkt")

(provide
 (all-defined-out)
; k-coloring      ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
; valid-coloring? ; (-> graph/c coloring/c boolean?)
)

; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (for*/and ([(e n) (in-indexed graph)] [child e])
     (not (= (color-ref coloring n) (color-ref coloring child)))))

; Returns a coloring/c if the given graph can 
; be colored with k colors.  Otherwise returns #f.
; The procedure accepts an optional solver% arugment.
; If not specified, the solver defaults to lingeling.
; Notes - Greg Nelson
; TODO:
; I added a trivial symmetry breakage heuristic, based on the problem
; I take the node with the highest degree,
;  and assert it will have the first color
; I then take its highest degree neighbor
;  and assert it will have the second color
; Add another symmetry breakage by degree of node
(define (k-coloring graph k [solver (lingeling)])
  
  
  
  
  
  (error 'k-coloring "not implemented yet!"))


; all-nodes-colored
(define (all-nodes-colored graph k)
  (map cnf-node-has-at-least-one-color (nodes graph))
  )

(define (make-hash-node-color-to-variable graph k)
  ;(make-hash (map cons (all-node-color-pairs graph k) (in-range
  (let ([pairs (all-node-color-pairs graph k)])
    (for/hash ([key pairs]
             [v (in-range (length pairs))])
    (values key v))))
    ; actually make this make a list of pairs, one list goes one way the other the other
    ; THEN test
    ; then put into let assignments into the main section above
    ; then wrap them with (var-for-node-color) or just access hash directly
    ; then test on the small graph example with the solver,
    ;   expanding the different sections by manually expanding the NANDs for edges


(define (all-node-color-pairs graph k)
  (for/fold ([r (list )])
            ([n (nodes graph)])
    (append r (node-color-pairs n k)))
  )

(define (node-color-pairs n k)
  (for/list ([c (in-range k)])
    (list n c)))

; no-neighbors-share-colors
(define (cnf-node-has-at-least-one-color node k size)
  (map (lambda (c) (node-colored-with-k node c size)) (in-range k)))

(define (node-colored-with-k node c size)
  ; list-from-to node*size node*size+k
  (* (+ node 1) (+ c 1))
  )


(node-colored-with-k 1 2 3)
