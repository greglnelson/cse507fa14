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
  (define offset (find-offset 1 k))
  (define proxy-var-start (* 10 (to-lit (node-count graph) k offset)))
  (define cnf (make-cnf graph k offset proxy-var-start))
  ;(print "pre-solve")
  ;(log-gc "pre-solve")
  (define solution (time (solve cnf)))
  ;(print solution)
  ;(log-gc "post-solve")
  ;(define reverse-lookup (time (reverse-hash lookup)))
  ;(log-gc "pre-coloring")
  (define the-coloring (time (node-coloring graph solution offset proxy-var-start)))
  ;(log-gc "post-coloring")
  the-coloring)
  
  
(define (log-gc comment)
  (printf "pre ~a\n" comment )
  (time (collect-garbage))
  (printf "post ~a\n" comment ))
  
  ;(error 'k-coloring "not implemented yet!"))

(define (make-cnf graph k offset proxy-var-start)
  (append
   (cnf-all-nodes-colored graph k offset)
   (cnf-no-edges-share-colors graph k offset proxy-var-start )))


(define (find-offset l k)
  (if (<= l k)
      (find-offset (* 10 l) k)
      l)
  )

(define (to-lit node c offset)
  (+ (* (+ 1 node) offset) c))

(define (deref-node lit offset)
  (- (quotient lit offset) 1))

(define (deref-color lit offset)
  (- lit (* (+ (deref-node lit offset) 1) offset)))




(define (node-coloring graph interp offset first-proxy-var)
  (define coloring (make-vector (node-count graph))) ; todo speedup by ignore vars after graph size + 1
  (let ([is-a-node-coloring (lambda (lit) (< lit first-proxy-var))])
    (for ([literal interp])
      #:break (not (is-a-node-coloring literal))
      (cond [(positive? literal) 
            (vector-set! coloring (deref-node literal offset) (deref-color literal offset))])))
          
  coloring)

; in index
; apply append pattern to reduce lists
; sequences in racket documentation - so use the 

; all-nodes-colored
(define (cnf-all-nodes-colored graph k offset)
  (for/list ([n (nodes graph)])
    (cnf-node-has-at-least-one-color n k offset)))

(define (cnf-node-has-at-least-one-color node k offset)
  (for/list ([c  (in-range k)])
    (node-colored-with-k node c offset)))

(define (node-colored-with-k node c offset)
  ;(hash-ref lookup (list node c))
  (to-lit node c offset)
  )

; no-neighbors-share-colors
(define (cnf-no-edges-share-colors graph k offset proxy-var-start)
  (apply append (for/list
            ([e (edges graph)])
            ; [proxy-offset (for/list ([x (in-range (edge-count graph))]) x)])
    (cnf-edge-doesnt-share-color (car e) (cdr e) k offset proxy-var-start))))

(define (cnf-edge-doesnt-share-color node1 node2 k offset proxy-var-start)
  ;(for/list ([c (in-range k)])
  ;  (cnf-xor node1 node2 c lookup proxy-var-counter)))
  (for/list ([c (in-range k)])
    (cnf-nand node1 node2 c offset proxy-var-start)))


(define (cnf-nand node1 node2 k offset proxy-var-start)
  (let (;[p (proxy-var node1 node2 k offset proxy-var-start)]
        [n1 (to-lit node1 k offset)]
        [n2 (to-lit node2 k offset)])
    ;(list ;(list    p                )
          (list (- n1) (- n2) )))
         ; (list    p     n1         )
         ; (list    p            n2  ))))



;(define (recurse-edges 

; deprecated code ---------------------------------------============================

(define (make-hash-node-color-to-variable graph k)
  ;(make-hash (map cons (all-node-color-pairs graph k) (in-range
  (let ([pairs (all-node-color-pairs graph k)])
    (for/hash ([key pairs]
             [v (in-range 1 (+ (length pairs) 1) 1)])
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

(define (reverse-hash h)
  (make-hash (hash-map h (lambda (k v) (cons v k))))
  )

(define (node-coloring-debug graph interp lookup first-proxy-var)
  (define rl (reverse-hash lookup)) ; vector
  (define coloring (make-vector (node-count graph))) ; todo speedup by ignore vars after graph size + 1
  (let ([is-a-node-coloring (lambda (lit) (and (positive? lit) (< lit first-proxy-var)))])
    (map (lambda (literal)
         (cond
           [(is-a-node-coloring literal) (let ([node-color (hash-ref rl literal)]) 
                                  (printf " (car node-color)~a (cdr node-color)~a" (car node-color) (car (cdr node-color))))])) interp))
  coloring)

#|
(define (node-coloring graph interp lookup first-proxy-var)
  (define rl (reverse-hash lookup)) ; vector
  (define coloring (make-vector (node-count graph))) ; todo speedup by ignore vars after graph size + 1
  (let ([is-a-node-coloring (lambda (lit) ((< lit first-proxy-var)))])
    (for ([literal interp])
      #:break (not (is-a-node-coloring literal))
      (cond [(positive? literal) 
          (let ([node-color (hash-ref rl literal)])
            (vector-set! coloring (car node-color) (car (cdr node-color))))])))
          
  coloring)
|#

#|
(define (node-coloring graph interp lookup first-proxy-var)
  (define rl (reverse-hash lookup)) ; vector
  (define coloring (make-vector (node-count graph))) ; todo speedup by ignore vars after graph size + 1
  (let ([is-a-node-coloring (lambda (lit) (and (positive? lit) (< lit first-proxy-var)))])
    (map (lambda (literal)
         (cond
           [(is-a-node-coloring literal) (let ([node-color (hash-ref rl literal)]) 
                                  (vector-set! coloring (car node-color) (car (cdr node-color))))])) interp))
  coloring)
|#
  
; not used
(define (cnf-xor node1 node2 k lookup proxy-var-counter)
  (let ([p (proxy-var node1 node2 k lookup proxy-var-counter)]
        [n1 (node-colored-with-k node1 k lookup)]
        [n2 (node-colored-with-k node2 k lookup)])
    (list (list    p                )
          (list (- p) (- n1) (- n2) )
          (list (- p)    n1     n2  )
          (list    p  (- n1)    n2  )
          (list    p     n1  (- n2) ))))

; from http://stackoverflow.com/questions/8508845/static-variables-in-scheme-racket
(define (make-counter n)
  (lambda ()
    (set! n (add1 n))
    n))


(define (proxy-var node1 node2 k offset proxy-var-start)
  (+ k proxy-var-start))