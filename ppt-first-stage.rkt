; Copyright ION - IRINEL DINU 2022
#lang racket

(provide (all-defined-out))

;; A primitive Pythagorean triplet (TPP) consists of
;; 3 nonzero natural numbers a, b, c with properties:
;; a ^ 2 + b ^ 2 = c ^ 2
;; a, b, c prime between them
;;
;; TPPs can be generated as a tree (infinite) with
;; the root (3,4,5), based on 3 matrix transformations:
;;
;; | -1 2 2 | | 1 2 2 | | 1 -2 2 |
;; T1 = | -2 1 2 | T2 = | 2 1 2 | T3 = | 2 -1 2 |
;; | -2 2 3 | | 2 2 3 | | 2 -2 3 |
;;
;; (3,4,5)
;; ______________ | ______________
;; | | |
;; (15,8,17) (21,20,29) (5,12,13)
;; ______ | ______ ______ | ______ ______ | ______
;; | | || | || | |
;; (35,12,37) ..........................................
;;
;; where:
;; (15, 8.17) = T1 · (3,4,5)
;; (21,20,29) = T2 · (3,4,5)
;; (5,12,13) ​​= T3 · (3,4,5) etc.
;;
;; In this representation, TPPs are indexed "from top to bottom",
;; respectively "from left to right", resulting in the order:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) ​​(35,12,37) ... etc.

;; Representation of matrices T1, T2, T3 as list lists:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '((1 2 2) (2 1 2) (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))
(define PPT '(3 4 5))

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

;; The function that calculates the difference used to determine each interval
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
    ; add the transformation type and recursively call the function for the new range
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

; The function that calculates the triplet resulting from the application
; transformations from Ts (T1, T2, T3) on a starting triplet ppt
(define (apply-matrix-transformations Ts ppt)
  (cond
    ; if the list of transformations no longer contains any
    ; element, recursion stops
    ((null? ts) ppt)
    ; apply each transformation on the initial ppt depending on
    ; this type, and the desired result is obtained gradually
    ; the recursion used is on the tail, because ppt holds the place
    ; of a battery and the calculations are performed in advance
    ((= (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
    ((= (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
    ((= (car Ts) 3) (apply-matrix-transformations (cdr Ts) (multiply T3 ppt)))))

;; The function that calculates the nth TPP in the tree
(define (get-nth-ppt-from-matrix-transformations n)
  ; using the get-tranformations function you get the list of necessary transformations
  ; and then apply each transformation to PPT (3 4 5) obtaining the desired triplet
  (apply-matrix-transformations (get-transformations n) PPT))
  