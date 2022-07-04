; Copyright DINU ION IRINEL 2022
#lang racket

(require "ppt.rkt")

(provide (all-defined-out))

;; Starting from the numbers e and f in the GH quartets, they can
;; obtain three solutions (a1, b1, c1), (a2, b2, c2), (a3, b3, c3)
;; which respects the relation a ^ 2 + b ^ 2 = c ^ 2.
;; These triplets do not represent TPP, and in some cases
;; contain negative coefficients, but this does not us
;; prevents us from using them to create the key
;; encryption / decryption for a simple cryptosystem.

;; The three solutions are obtained according to the formulas below:
(define (a1 and f) (+ (* e e) (* 2 f e)))
(define (b1 and f) (+ (* 2 f f) (* 2 f e)))
(define (c1 and f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 and f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 and f) (* 2 f and))
(define (c3 and f) (+ (* f f) (* e e)))

;; The check function is just a way to:
;; - view the triplets generated for 2 numbers e and f
;; - test that they respect the relation a ^ 2 + b ^ 2 = c ^ 2
(define (check and f)
  (let ((a1 (a1 and f)) (b1 (b1 and f)) (c1 (c1 and f))
                      (a2 (a2 and f)) (b2 (b2 and f)) (c2 (c2 and f))
                      (a3 (a3 e f)) (b3 (b3 e f)) (c3 (c3 e ​​f)))
    (display (list (list a1 b1 c1) (list a2 b2 c2) (list a3 b3 c3)))
    (newline)
    (and
     (= (+ (sqr a1) (sqr b1)) (sqr c1))
     (= (+ (sqr a2) (sqr b2)) (sqr c2))
     (= (+ (sqr a3) (sqr b3)) (sqr c3)))))

;; Example of use
;; (with the first 3 generations of the quartet tree):
;; (map check
;; '(1 1 2 2 1 4 4 2 5 5 2 3 3)
;; '(2 4 5 3 6 9 7 9 12 8 7 8 4))

;; We will send messages using the 26 characters from
;; English alphabet (lowercase only),
;; plus the "space" character. Consequently, we need to
;; we encode 27 distinct characters:
;; - associate code 0 with the character "space"
;; - we associate the codes 1-26 to the characters 'a', 'b' ... 'z'
;;
;; The two parties involved in the communication will send one
;; number n, based on which each will determine the key of
;; encryption / decryption as follows:
;; - determines the nth quartet in the TPP tree
;; - extract the values ​​e and f from this quartet
;; - the key will be (a1, b1, c1, a2, b2, c2, a3, b3, c3) mod 27
;; (each tuple value becomes a code between 0 and 26)


; Implement the function that receives a number n and
; get the encryption / decryption key according to the algorithm
; above.

; I used the let * statement to keep the list
; obtained from the get-nth-quadruple call, I determined
; e and f and then we would apply the modulo 27 function to each element
; from the list obtained based on the formulas
(define (key n)
  (let * (list-quadruple (get-nth-quadruple n))
         (e (list-quadruple frame))
         (f (caddr list-quadruple)))
    (map (λ (x) (module x 27)) (list (a1 and f) (b1 and f) (c1 and f)
                                    (a2 and f) (b2 and f) (c2 and f)
                                    (a3 and f) (b3 and f) (c3 and f)))))

; Implement the function that receives a message (a string of
; characters containing only lowercase letters and spaces) and
; returns a list of codes associated with that message
; (space becomes 0, 'a' becomes 1 ... 'z' becomes 26).

; During the solution I applied a lambda function using
; map, in which I turned the characters into codes,
; we treated the case specific to the space and determined the final list
(define (message-> message codes)
  (map (λ (x)
         (if (= (char-> integer x) 32)
             (- (char-> integer x) 32)
             (- (char-> integer x) 96)))
       (string-> list message)))


; Implement the function that receives a list of codes
; (numbers between 0 and 26) and returns the associated message
; (the reverse process of the message-> codes function).

; Similar to the above function, I transformed each
; code in the specific character, we treated the case for the character
; space and finally I built the list with the obtained string
(define (codes-> message codes)
  (list-> string (map (λ (x)
                       (if (= x 0)
                           (integer-> char (+ x 32))
                           (integer-> char (+ x 96))))
                     codes)))


;; For encryption and decryption operations, we work
;; with codes between 0 and 26.
;; We use the following notations:
;; m = a code from the representation of the original message
;; c = a code from the representation of the encrypted message
;; k = a code from the key representation
;;
;; In order to perform encryption / decryption operations,
;; the key must be extended / truncated to size
;; the message to be encrypted / decrypted.
;;
;; Furthermore, the encryption is performed according to the formula:
;; c = (m + k) mode 27 (for each index)
;;
;; Similarly, the formula for decryption is:
;; m = (c - k) mode 27 (for each index)

; Implement the function that receives a key and a key
; size size and extends / truncates the key to
; size size.


; Ex:
; (extend-key '(24 16 20 11 3 7 21 20 2) 7) ⇒' (24 16 20 11 3 7 21)
; (extend-key '(24 16 20 11 3 7 21 20 2) 21)
; 21/9 = 3
; ⇒ '(24 16 20 11 3 7 21 20 2 24 16 20 11 3 7 21 24 16 20 11 3 7 21 20 2)
; 27 - 21 = 6
; ⇒ '(24 16 20 11 3 7 21 20 2 24 16 20 11 3 7 21 24 16 20)


; the flag specific to expanding a list
(define extend-flag 1)
; the flag specific to truncating a list
(define trunc-flag 0)

; The function that deals with changing the list accordingly
; of the flag operations received as a parameter
(define (change-list key step flag)
  (let iter ((i 0) (result-list key))
    (if (<= step i)
        result-list
        (iter (+ i 1)
              (if (= 0 flag)
                  ; deleting the last item in the list
                  (reverse (cdr (reverse result-list)))
                  ; adding a new list to the current list
                  (append result-list key))))))
           

; The function of expanding a list represented as a key
(define (extend-key key size)
  (if (<(length key) size)
      ; expand the list by adding new lists, then deleting
      ; items that are extra
      (let ((list-extended (change-list key (quotient size (length key)) extend-flag))))
        (change-list list-extended (- (length list-extended) size) trunc-flag))
      ; removing items from the list
      (change-list key (- (length key) size) trunc-flag)))


; EVERYTHING
; Notice that the encryption / decryption algorithms are
; it looks a lot like. Abstract the process into a
; more general function (try to give it a name
; suggestive) from which you then derive, with minimal effort,
; functions:
; (encrypt-codes message key)
; (decrypt-codes crypted key)
; Do not use explicit (but functional) recursion.

; Both functions receive two lists of codes (representing
; clear / encrypted message, respectively key) and return a list
; of codes (encrypted / decrypted message, as appropriate).

; Functions used to encrypt and decrypt a
; message, respectively of a code received as an argument

; Encryption function
(define encrypt-function
  (λ (m k) (module (+ m k) 27)))

; Decryption function
(define decrypt-function
  (λ (m k) (module (- m k) 27)))

; The abstraction function that deals with the processing of a
; code received as argument (encryption / decryption)
(define (operate-code code key)
  (λ (function) (map function code (extend-key key (length code)))))

; Partial application of the code encryption function
(define (encrypt-codes message key)
  ((operate-code message key) encrypt-function))

; Partial application of a code decryption function
(define (decrypt-codes crypted key)
  ((operate-code crypted key) decrypt-function))


; The abstraction function that deals with the processing of a
; message received as argument (encryption / decryption)
(define (operate-message message key)
  (λ (function)
    (codes-> message ((operate-code (message-> message codes) key) function))))

; Partial application of the message encryption function
(define (encrypt-message message key)
  ((operate-message message key) encrypt-function))

; Partial application of the message decryption function
(define (decrypt-message crypted key)
  ((operate-message crypted key) decrypt-function))
  