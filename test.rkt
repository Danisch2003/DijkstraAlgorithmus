#lang racket

(struct Graph (startnode nodelength neighbors weights))

(define (dijkstra graph)
  ;(define-values (a b c) (init 0 graph '() '() '()))
  ;(display (init 0 graph '() '() '()))
  (nextNode graph (list-ref (init 0 graph '() '() '()) 0) (list-ref (init 0 graph '() '() '()) 1)
  (list-ref (init 0 graph '() '() '()) 2)))

(define (append-element lst elem)
  (append lst (list elem)))

(define (init currnode graph distance previous FREE) ;make from currnode -> node
  (cond 
    [(= currnode (Graph-nodelength graph)) (list FREE distance previous)]
    [(= currnode (Graph-startnode graph)) (init (+ currnode 1) graph (append-element distance 0) (append-element previous 0) (append-element FREE currnode))]
    [(init (+ currnode 1) graph (append-element distance 9999) (append-element previous null) (append-element FREE currnode))]))

(define (getNodeWithMinDist FREE distance minNode minDist)
  (cond 
  [(= 0 (length FREE)) minNode]
  [(cond 
    [(>= (list-ref distance (car FREE)) minDist) (getNodeWithMinDist (cdr FREE) distance minNode minDist)]
    [(getNodeWithMinDist (cdr FREE) distance (car FREE) (list-ref distance (car FREE)))]
  )]))

(define (removeNodeWithMinDist FREE distance)
  (remove (getNodeWithMinDist FREE distance 9999 9999) FREE)
)

(define (nextNode graph FREE distance previous)
  (cond
    [(empty? FREE) previous]
    [
      (nextNode graph 
      (removeNodeWithMinDist FREE distance) 
      (list-ref (updateNeighbors 0 (getNodeWithMinDist FREE distance 9999 9999) graph FREE distance previous) 0)
      (list-ref (updateNeighbors 0 (getNodeWithMinDist FREE distance 9999 9999) graph FREE distance previous) 1))
    ]))

(define (elementInList elem lst)
  (cond 
    [(empty? lst) #f]
    [(= (car lst) elem) #t]
    [(elementInList elem (cdr lst))]))

(define (updateNeighbors idx minimumnode graph FREE distance previous)
  (cond 
    [(= idx (length (list-ref (Graph-neighbors graph) minimumnode))) (list distance previous)] 
    [
      (elementInList (list-ref (list-ref (Graph-neighbors graph) minimumnode) idx) FREE)
      (updateNeighbors (+ 1 idx) minimumnode graph FREE 
      (list-ref (updateDistance idx minimumnode graph (list-ref (list-ref (Graph-neighbors graph) minimumnode) idx) distance previous) 0)
      (list-ref (updateDistance idx minimumnode graph (list-ref (list-ref (Graph-neighbors graph) minimumnode) idx) distance previous) 1))
    ]))

(define (updateDistance idx minimumnode graph neighbor distance previous)
  (cond
    [
      (< (+ (list-ref distance minimumnode) (list-ref (list-ref (Graph-weights graph) minimumnode) idx)) (list-ref distance neighbor))
      (list 
      (list-set distance neighbor (+ (list-ref distance minimumnode) (list-ref (list-ref (Graph-weights graph) minimumnode) idx)))
      (list-set previous neighbor minimumnode))
    ]
    [(list distance previous)]))
    

(define g (Graph 0 3 
'((1 2) (2) (0)) 
'((1 6) (2) (2))
))
(dijkstra g)

; DEBUGGING...
;(updateNeighbors 0 1 g '(0 1 2 3) '(0 9999 9999 9999) '(() () () ()))
;(updateDistance 0 0 g 1 '(0 9999 9999 9999) '(() () () ()))
