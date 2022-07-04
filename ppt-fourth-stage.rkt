; Copyright DINU ION IRINEL 2022

#lang racket

(provide (all-defined-out))

;; If we're only interested in the nth TPP in the tree, it's
;; convenient to determine the sequence of transformations that
;; leads to this TPP, as we have done so far.
;;
;; Conversely, if we are interested in the first n TPP (or in
;; generally a longer sequence of TPP) would be preferable
;; creating an infinite flow that contains them all
;; in order.
;;
;; We notice that this order corresponds to a scrolling
;; BFS of the infinite tree. This is a simpler BFS
;; than the usual BFS
;; (https://en.wikipedia.org/wiki/Breadth-first_search),
;; whereas the successors of a TPP are automatically new triplets,
;; so it is not necessary to check if a node has more
;; visited or not.
;;
;; The scheme of this simplified BFS is:
;; 1. we initialize the queue of nodes to be visited with
;; tree root (triplet (3,4,5))
;; 2. add the first node in the queue in the result
;; 3. we add its 3 successors in the tail of knots
;; to be visited
;; 4. we return to step 2 (since we build a flow
;; infinity, there is no stopping condition, and all
;; structures are flows: both tail and
;; BFS function result)

;; Definition of matrices T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '((1 2 2) (2 1 2) (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

(define initial-tuple '(3 4 5))


;; The dot-product function calculates the scalar product of two vectors X and Y
;; Ex: (-1,2,2) · (3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  ; if we have reached the end of a list, the recursion stops and returns 0
  (if (null? X)
      0
      ; otherwise we add to the sum the product of the two numbers at the top of the lists
      ; recursion continues on the rest of the items in the lists
      ; the recursion used is on the stack because the calculations
      ; it is made on the return from recursion
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))


;; The functions that calculate the product between a matrix M and a vector V
;; Ex: | -1 2 2 | | 3 | | 15 |
;; | -2 1 2 | · | 4 | = | 8 |
;; | -2 2 3 | | 5 | | 17 |

;; Helper function to use recursion on the queue
(define (multiply-help M V result_list)
  ; if the matrix M no longer contains lines
  ; the recursion stops and the desired result is returned
  (if (null? M)
      (reverse result_list)
      ; otherwise I recursively apply the dot-product function to
      ; each line in the matrix and the vector V, obtaining the desired list
      ; the recursion used is on the queue because result_list
      ; represents an accumulator and the calculations are performed in advance on recursion
      (multiply-help (cdr M) V (cons (dot-product (car M) V) result_list))))

(define (multiply M V)
  (multiply-help M V null))


;; The function that determines the successors of a node
;; T1 T2 T3 transformations are applied
(define successors
  (λ (s) (stream-map (λ (x) (multiply x s)) (stream T1 T2 T3)))


;; The function that determines the infinite flow of TPP using
;; the BFS traversal algorithm of the infinite tree
;; I used a named let statement to simulate the queue

(define (breadth-first-search initial-list find-successors)
  ; queue initialization
  (let search ([queue (stream-cons initial-list empty-stream)])
    (if (stream-empty? queue) empty-stream
        ; extracting the first element from the queue and adding
        ; neighboring elements at the end of the queue
        (stream-cons (stream-first queue)
                     (search (stream-append (stream-rest queue))
                                            (find-successors (stream-first queue)))))))

(define ppt-stream-in-tree-order
  (breadth-first-search initial-tuple successors))


;; Another way to generate TPP is to use pairs (g, h)
;; who meet the conditions:
;; g, h odd
;; g <h
;; g, h prime between them
;;
;; It is no coincidence that I chose these notations, the theory is the same
;; with the one behind the quartets (g, e, f, h), which they
;; we can also express as (g, (h-g) / 2, (h + g) / 2, h).
;;
;; To obtain a TPP from a pair (g, h) apply
;; the same formulas (but we will express them according to g and h):
;; a = gh
;; b = 2ef = (h - g) (h + g) / 2
;; = (h ^ 2 - g ^ 2) / 2
;; c = e ^ 2 + f ^ 2 = (h - g) ^ 2/4 + (h + g) ^ 2/4
;; = (h ^ 2 + g ^ 2) / 2
;;
;; This generation mode provides us with TPP in another order
;; than that given by the width of the TPP shaft.
;;
;; The new order is obtained by going through the diagram columns:
;; h
;; 3 5 7 9 11. . .
;; 1 (1.3) (1.5) (1.7) (1.9) (1.11). . .
;; 3 (3.5) (3.7) - (3.11). . .
;; 5 (5.7) (5.9) (5.11). . .
;; g 7 (7.9) (7.11). . .
;; 9 (9.11). . .
;; . . . .
;; . . .
;; . .
;; (missing pair (3,9), 3 and 9 not being prime to each other)
;;
;; Using this indexing, the first 6 TPPs are:
;; (3,4,5) - from the pair (1,3)
;; (5,12,13), (15,8,17) - of (1,5), (3,5)
;; (7,24,25), (21,20,29), (35,12,37) - of (1,7), (3,7), (5,7)
;;
;; We aim to define the infinite flow of TPP in the order of
;; above. It is based on the corresponding flow of
;; pairs (g, h), which we generate as follows:
;; - we start with 2 infinite flows:
;; * G = 1, 3, 5, 7 ...
;; * H = 3, 5, 7, 9 ... (since g <h)
;; - the orderly flow in columns will contain:
;; * the pair composed of the smallest numbers in G and H
;; (ex: (1,3))
;; * then the interclassification (according to the order "on columns") between:
;; - the pairs composed of the minimum in G and the rest in H
;; (ex: (1,5), (1,7), (1,9) ...)
;; - the ordered flow generated by the remainder of G and the remainder of H
;; (ex: (3,5), (3,7), (5,7) ...)
;; This is the general approach, after which we generate everything
;; pairs, including those of non-prime numbers
;; between them. Non-compliant pairs must be removed later
;; (using the gcd library function).


;; Definition of the function that receives 2 infinite numerical flows
;; G and H, and generates the flow of pairs of one element each
;; of G and one of H ordered according to the above method.
;; The conditions for g and h to be odd, prime to each other, respectively
;; maintaining the constraint g <h (as long as you follow the algorithm) no
;; must be imposed in the implementation of the peer function.
;; They will be ensured by defining the flows below by:
;; - calling pairs exclusively on streams
;; G = 1, 3, 5, 7 ... and H = 3, 5, 7, 9 ...
;; - elimination of pairs of odd numbers between them (which
;; exists as a result of the pairs function, but will no longer exist
;; in the gh-pairs-stream)

(define (get-initial-stream G H)
  (stream-cons (cons (stream-first G) (stream-first (stream-rest H))) (get-initial-stream G (stream-rest H))))

(define (max-pair pair)
  (max (car pair) (cdr pair)))

(define (go S1 S2)
  (let ([s1-pair (stream-first S1)] [s2-pair (stream-first S2)])
    (cond ((<= (max-pair s1-pair) (max-pair s2-pair))
           (stream-cons s1-pair (merge (stream-rest S1) S2))
          ((stream-cons s2-pair (merge S1 (stream-rest S2))))))

(define (pairs G H)
  (stream-cons (cons (stream-first G)) (stream-first H))
               (merge (get-initial-stream G H) (pairs (stream-rest G) (stream-rest H))))

; The function that determines the flow of natural numbers
(define (naturals-from n)
  (let loop (seed n))
    (stream-cons seed (loop (add1 seed))))

(define naturals (naturals-from 0))


; Determining the flow of odd numbers
; Ex: 1 3 5 7 9 11
(define odd-numbers-stream (stream-filter odd? naturals))

;; The function that determines the flow of pairs (g, h) on which
;; bases the new TPP indexing.
(define gh-pairs-stream
  (stream-filter (λ (x) (= (gcd (car x) (cdr x)) 1))
                 (pairs odd-numbers-stream (stream-rest odd-numbers-stream)))

;; The function that determines the TPP flow corresponding to the flow
;; previous pair (g, h).
;; a = gh
;; b = 2ef = (h - g) (h + g) / 2
;; = (h ^ 2 - g ^ 2) / 2
;; c = e ^ 2 + f ^ 2 = (h - g) ^ 2/4 + (h + g) ^ 2/4
;; = (h ^ 2 + g ^ 2) / 2

(define (ppt-stream-helper S)
  (let * ((first-element (stream-first S)))
         (g (car first-element))
         (h (cdr first-element))
         (a (* g h))
         (b (/ (- (expt h 2) (expt g 2)) 2))
         (c (/ (+ (expt h 2) (expt g 2)) 2)))
    (stream-cons (list a b c) (ppt-stream-helper (stream-rest S))))

(define ppt-stream-in-pair-order
  (ppt-stream-helper gh-pairs-stream))
