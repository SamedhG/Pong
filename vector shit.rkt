;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |vector shit|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct Vector [x y])


; vector vector -> Number
; dot product takes 2 vector and gives dot product
(define (dot-product vector1 vector2)
  (+
   (* (vector-x vector1) (vector-x vector2))
   (* (vector-y vector1) (vector-y vector2))))

; vector Number -> Vector
; multiply vector by x
(define (multiply-vector-by Vector Num)
  (make-vector
   (* (vector-x Vector) Num)
   (* (vector-y Vector) Num)))

; vector vector -> Vector
; vector1 - vector2
(define (subtract-vector vector1 vector2)
  (make-vector
   (- (vector-x vector1) (vector-x vector2))
   (- (vector-y vector1) (vector-y vector2))))

; vector -> Number
; gives magnitude
(define (vector-magnitude vector1)
  (sqrt (dot-product vector1 vector1)))

;posn posn -> vector
;this function is a joke
(define (posn-posn2vector Posn1 Posn2)
  (make-vector (subtract-posn Posn1 Posn2)))

;posn posn -> number
(define (distance Posn1 Posn2) (magnitude (posn-posn2vector Posn1 Posn2)))
               
; posn posn -> posn
; posn1 - posn2
(define (subtract-posn posn1 posn2)
  (make-posn
   (- (posn-x posn1) (posn-x posn2))
   (- (posn-y posn1) (posn-y posn2))))
   