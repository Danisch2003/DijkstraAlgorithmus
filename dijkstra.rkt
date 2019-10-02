#lang racket

(struct Graph (startnode nodelength neighbors weights))

(define (dijkstra graph)
  (display (init 0 graph '() '() '()))
  (nextNode graph (list-ref (init 0 graph '() '() '()) 0) (list-ref (init 0 graph '() '() '()) 1)
  (list-ref (init 0 graph '() '() '()) 2)))

(define (append-element lst elem)
  (append lst (list elem)))

(define (init currnode graph distance previous FREE) ;make from currnode -> node
  (cond 
    [(= currnode (Graph-nodelength graph)) (list FREE distance previous)]
    [(= currnode (Graph-startnode graph)) (init (+ currnode 1) graph (append-element distance 0) (append-element previous null) (append-element FREE currnode))]
    [(init (+ currnode 1) graph (append-element distance 9999) (append-element previous null) (append-element FREE currnode))]))

(define (nextNode graph FREE distance previous)
  (cond
    [(empty? FREE) previous]
    [
      (print (removeNodeWithMinDist FREE distance)) ;FREE
      (nextNode graph 
      (removeNodeWithMinDist FREE distance) 
      (list-ref (updateNeighbors 0 (getNodeWithMinDist FREE distance 9999 9999) graph FREE distance previous) 0)
      (list-ref (updateNeighbors 0 (getNodeWithMinDist FREE distance 9999 9999) graph FREE distance previous) 1))
    ]))

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

(define (elementInList elem lst)
  (cond 
    [(empty? lst) #f]
    [(= (car lst) elem) #t]
    [(elementInList elem (cdr lst))]))
  
(define (updateDistance idx graph minimumnode neighbor distance previous)
  (cond
    [
      (< (+ (list-ref distance minimumnode) (list-ref (list-ref (Graph-weights graph) minimumnode) idx)) (list-ref distance neighbor))
      (list 
      (list-set distance neighbor (+ (list-ref distance minimumnode) (list-ref (list-ref (Graph-weights graph) minimumnode) idx)))
      (list-set previous neighbor minimumnode))
    ]
    [(list distance previous)]
  )
 )

(define (updateNeighbors idx minimumnode graph FREE distance previous)
  (cond 
    [(= idx (length (list-ref (Graph-neighbors graph) minimumnode))) (list distance previous)] 
    [
      (elementInList (list-ref (list-ref (Graph-neighbors graph) minimumnode) idx) FREE)
      (updateNeighbors (+ 1 idx) minimumnode graph FREE 
      (list-ref (updateDistance idx graph minimumnode (list-ref (list-ref (Graph-neighbors graph) minimumnode) idx) distance previous) 0)
      (list-ref (updateDistance idx graph minimumnode (list-ref (list-ref (Graph-neighbors graph) minimumnode) idx) distance previous) 1))
      ;update Distanz
    ]))

(define g (Graph 0  4 '((1 2) (0 3) (0) (1 0)) '((2 3) (2 1) (3) (1 0))))
(dijkstra g)

;(updateNeighbors 0 1 g '(0 1 2 3) '(0 9999 9999 9999) '(() () () ()))
;(updateDistance 0 g 0 1 '(0 9999 9999 9999) '(() () () ()))
