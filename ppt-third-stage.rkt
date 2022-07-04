; Copyright DINU ION IRINEL 2022
#lang racket
(provide (all-defined-out))

;; The same TPP tree determined by application
;; transformations T1, T2, T3 can be generated using
;; GH (Gopal-Hemachandra) tuples.
;;
;; For any pair (g, e), the sequence GH is:
;; g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; For (g, e) = (1, 1) we obtain the Fibonacci sequence.
;;
;; The first 4 numbers in the sequence form the GH quartet:
;; (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; For such a quartet (g, e, f, h), we define:
;; a = gh, b = 2ef, c = e ^ 2 + f ^ 2
;; and we can prove that (a, b, c) is a Pythagorean triplet.
;;
;; (a, b, c) is the TPP itself, if we add the conditions:
;; g, e, f, h prime between them
;; g odd
;; but you will not need to do such checks,
;; since we have at our disposal an algorithm that generates
;; exclusively TPP.
;;
;; This algorithm is very similar to the one in the stage
;; previous, with the following differences:
;; - the nodes in the tree are quartets, not triplets
;; (from the quartet we obtain a TPP according to the formulas)
;; (ex: (1,1,2,3) => (1 * 3,2 * 1 * 2,1 ^ 2 + 2 ^ 2) = (3,4,5))
;; - we get the next generation of quartets using
;; three transformations Q1, Q2, Q3 for quartets, instead
;; of T1, T2, T3 who worked with triplets
;;
;; Q1 (g, e, f, h) = (h, e, h + e, h + 2e)
;; Q2 (g, e, f, h) = (h, f, h + f, h + 2f)
;; Q3 (g, e, f, h) = (g, f, g + f, g + 2f)
;;
;; The resulting tree looks like this:
;;
;; (1,1,2,3)
;; ______________ | ______________
;; | | |
;; (3,1,4,5) (3,2,5,7) (1,2,3,5)
;; ______ | ______ ______ | ______ ______ | ______
;; | | || | || | |
;; (5,1,6,7) .........................................

; Definition of functions Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

; Definition of matrices T1 T2 T3
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '((1 2 2) (2 1 2) (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

(define initial_tuple_trip '(3 4 5))
(define initial_tuple_cvar '(1 1 2 3))

; The function that calculates the scalar product of two vectors X and Y uses functional
; It is guaranteed that X and Y have the same length.
; Ex: (-1,2,2) · (3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (apply + (map * X Y)))


; The function that calculates the product between a matrix M
; and a vector V using functional
; It is guaranteed that M and V have compatible dimensions.
; Ex: | -1 2 2 | | 3 | | 15 |
; | -2 1 2 | · | 4 | = | 8 |
; | -2 2 3 | | 5 | | 17 |
(define (multiply M V)
  (map (λ (row) (dot-product V row)) M))


;; The functions on the basis of which the list of transformations is determined
;; by which it is obtained, starting from the nth triplet (3,4,5)
;; TPP from the tree

;; The function below determines a list that contains the first
;; position the level of the desired ppt, on the second position the maximum index of
;; the respective level and on the last position the minimum index
;; => list = [level, max_value, min_value]
(define (level-max-min n desired_level max_value min_value)
  ; if the calculated ppt sum exceeds the value of position n
  ; the recursion is stopped and the obtained list is returned
  (if (> = max_value n)
      (list desired_level max_value (- min_value))
      ; otherwise the level is incremented and calculated
      ; maximum value (amount) and minimum value recursively
      (level-max-min n
                     (+ 1 desired_level)
                     (+ max_value (expt 3 desired_level))
                     (- max_value (expt 3 desired_level)))))

;; The function that calculates the difference is specific to each interval
(define (division trasnformations_list)
  ; the difference between the maximum index and the minimum index is realized
  ; and the result is divided by 3 (it is desired to obtain three intervals)
  (/ (+ 1 (- (frame trasnformations_list) (caddr trasnformations_list))) 3))


(define (transformations n result_list t_list)
  (cond
    ; if the length of the list is less than 1 than level
    ; the recursion is stopped and the result list is returned
    ((= (+ (length result_list) 1) (car t_list)) (reverse result_list))

    ; if the limit condition of the first interval is met
    ; the function for the new interval is called recursively
    ; proceed similarly for the other two intervals
    ; the recursion on the tail is used, the result being determined in advance
    
 ; [min min + division - 1]
    ; if the first interval is valid, transform 1 is added to the result
    ((and (> = n (caddr t_list)) (<= n (- (+ (caddr t_list) (division t_list)) 1)))
     (transformations n (cons 1 result_list) (list (car t_list)
                                                  (- (+ (caddr t_list) (division t_list)) 1)
                                                  (caddr t_list))))
    ; [min + division min + 2 * division - 1]
    ; if the second interval is valid add transformation 2 to the result
    ((and (> = n (+ (caddr t_list) (division t_list))) (<= n (- (+ (caddr t_list) (* 2 (division t_list))) 1)))
     (transformations n (cons 2 result_list) (list (car t_list)
                                                  (- (+ (caddr t_list) (* 2 (division t_list))) 1)
                                                  (+ (caddr t_list) (division t_list)))))
    ; [min + 2 * division min + 3 * division - 1]
    ; if the third interval is valid add transformation 3 to the result
    ((and (> = n (+ (caddr t_list) (* 2 (division t_list)))) (<= n (- (+ (caddr t_list) (* 3 (division t_list))) 1)))
     (transformations n (cons 3 result_list) (list (car t_list)
                                                   (- (+ (caddr t_list) (* 3 (division t_list))) 1)
                                                   (+ (caddr t_list) (* 2 (division t_list)))))))
(define (level n)
  (level-max-min n 0 0 0))

(define (get-transformations n)
  (transformations n null (level n)))



; The function that applies the transformations (functions) in the Fs list supersedes the initial tuple
(define (apply-functional-transformations Fs tuple)
  ; using foldl scrolls through the list of functions and
  ; apply each function on tuple (for abstraction I used the lambda function)
  (foldl (λ (f x) (f x)) tuple Fs))


; The function that creates a list of functions based on the list of transformations received as a parameter
; Ex: T = '(1 3 2 3) => L =' (F1 F3 F2 F3)
(define (create_functions T list_functions F1 F2 F3)
  (cond
    ; if the list of transformations no longer contains any
    ; element, recursion stops
    ((null? T) list_functions)
    ; in case of contract, the position is added in the final list depending on its type
    ((= (car T) 1) (create_functions (cdr T) (cons F1 list_functions) F1 F2 F3))
    ((= (car T) 2) (create_functions (cdr T) (cons F2 list_functions) F1 F2 F3))
    ((= (car T) 3) (create_functions (cdr T) (cons F3 list_functions) F1 F2 F3))))


; The function that determines get-nth-tuple
(define (get-nth-tuple n tuple F1 F2 F3)
  (apply-functional-transformations (create_functions (reverse (get-transformations n)) null F1 F2 F3) tuple))

; The function that calculates the nth TPP in the tree using triplet transformations.
(define (get-nth-ppt-from-matrix-transformations n)
  ; partially apply the functions (multiply T)
  (get-nth-tuple n initial_tuple_trip (curry multiply T1) (curry multiply T2) (curry multiply T3)))

; Function that calculates the nth quartet in the tree, using the transformations on quartets.
(define (get-nth-quadruple n)
  ; partially apply functions (apply Q)
  (get-nth-tuple n initial_tuple_cvar (curry apply Q1) (curry apply Q2) (curry apply Q3))

; The function that calculates the nth TPP in the tree using
; the result returned by get-nth-quadruple
; For such a quartet (g, e, f, h), we define:
; a = gh, b = 2ef, c = e ^ 2 + f ^ 2
; and we can prove that (a, b, c) is a Pythagorean triplet.

(define (get-nth-ppt-from-GH-quadruples n)
  (list (* (car (get-nth-quadruple n)) (cadddr (get-nth-quadruple n))); a = g * h
        (* (* (cadr (get-nth-quadruple n)) (caddr (get-nth-quadruple n))) 2); b = 2 * e
        (+ (expt (cadr (get-nth-quadruple n)) 2) (expt (caddr (get-nth-quadruple n)) 2)))); c = e ^ 2 + f ^ 2
