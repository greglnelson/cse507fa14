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
  (define lookup (make-hash-node-color-to-variable graph k))
  (define proxy-var-counter (make-counter (+ 1 (hash-count lookup))))
  (define cnf (make-cnf graph k lookup proxy-var-counter))
  cnf)
  
  
  
  
  ;(error 'k-coloring "not implemented yet!"))

(define (make-cnf graph k lookup proxy-var-counter)
  (append
   (cnf-all-nodes-colored graph k lookup)
   (cnf-no-edges-share-colors graph k lookup proxy-var-counter )))


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

; all-nodes-colored
(define (cnf-all-nodes-colored graph k lookup)
  (for/list ([n (nodes graph)])
    (cnf-node-has-at-least-one-color n k lookup)))

(define (cnf-node-has-at-least-one-color node k lookup)
  (for/list ([c  (in-range k)])
    (node-colored-with-k node c lookup)))

(define (node-colored-with-k node c lookup)
  (hash-ref lookup (list node c))
  )

; no-neighbors-share-colors
(define (cnf-no-edges-share-colors graph k lookup proxy-var-counter)
  ;(for/list ([e (edges graph)])
  ;  (cnf-edge-doesnt-share-color (car e) (cdr e) k lookup proxy-var-counter)))
  (for/fold ([r (list )])
            ([e (edges graph)])
    (append r (cnf-edge-doesnt-share-color (car e) (cdr e) k lookup proxy-var-counter))))

(define (cnf-edge-doesnt-share-color node1 node2 k lookup proxy-var-counter)
  ;(for/list ([c (in-range k)])
  ;  (cnf-xor node1 node2 c lookup proxy-var-counter)))
  (for/fold ([r (list )])
            ([c (in-range k)])
    (append r (cnf-xor node1 node2 c lookup proxy-var-counter))))
  
(define (cnf-xor node1 node2 k lookup proxy-var-counter)
  (let ([p (proxy-var node1 node2 k lookup proxy-var-counter)]
        [n1 (node-colored-with-k node1 k lookup)]
        [n2 (node-colored-with-k node2 k lookup)])
    (list (list (- p) (- n1) (- n2) )
          (list (- p)    n1     n2  )
          (list    p  (- n1)    n2  )
          (list    p     n1  (- n2) ))))

; from http://stackoverflow.com/questions/8508845/static-variables-in-scheme-racket
(define (make-counter n)
  (lambda ()
    (set! n (add1 n))
    n))

(define (proxy-var node1 node2 k lookup proxy-var-counter)
  (proxy-var-counter))
  