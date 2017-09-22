;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pong Game v.3 Sept 22|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/universe)
(require 2htdp/image)

(define PONG-INITX 300)
(define PONG-INITY 150)

(define SCENE-WIDTH 600)
(define SCENE-HEIGHT 600)
(define SCENE (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "white"))

(define BALL-RADIUS 20)
(define BALL (circle BALL-RADIUS "solid" "blue"))

(define PADDLE-RADIUS 50)
(define PADDLE-SPEED 50)
(define PADDLE (circle PADDLE-RADIUS "solid" "red"))
(define PADDLE-X 600)

(define DIFFICULTY 0.3)

(define (SCORE-BOX score)
  (overlay
   (text (number->string score) 20 "black")
   (rectangle 40 25 "solid" "white")))
(define SCORE-POSITION (make-posn 50 50))


(define-struct vector [x y])
; a Vector is a (make-vector number number)


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
(define (posn-posn2vector posn1 posn2)
  (make-vector (- (posn-x posn1) (posn-x posn2))
               (- (posn-y posn1) (posn-y posn2))))

;posn posn -> number
;calculate distance between two posns
(define (distance Posn1 Posn2)
  (vector-magnitude (posn-posn2vector Posn1 Posn2)))
               
; posn posn -> posn
; posn1 - posn2
(define (subtract-posn posn1 posn2)
  (make-posn
   (- (posn-x posn1) (posn-x posn2))
   (- (posn-y posn1) (posn-y posn2))))


;; vector vector -> vector
;; reflect vec1 over vec2, and then reverse the vector's direction
(define (reflect-reverse vec1 vec2)
  (subtract-vector vec1
                   (multiply-vector-by vec2
                                       (/ (* 2 (dot-product vec1 vec2))
                                          (dot-product vec2 vec2)))))


;; ball paddle -> vector
;; modify the velocity of ball after bouncing off the circular paddle
(define (react-to-paddle b p)
  (cond
    [(ball-hit-wall b) (reflect-reverse (ball-vel b)
                    (posn-posn2vector (ball-loc b)
                                      (make-posn PADDLE-X p)))]
    [else (ball-vel b)]))



(define-struct ball [loc vel hit-wall])
;; loc is a (make-posn NNN NNN)
;; vel is a (make-vector number number)
;; hit-wall is a Boolean
;; NNN is a non-negative number


(define-struct game [ball paddle score])
;; a paddle is a NNN, the paddle's y-coord
;; a score is the NNN


(define PARAMETER 0.3)
(define (difficulty vel) (+ 1 (expt PARAMETER (vector-magnitude vel))))




(define (main game)
  
  (big-bang game

            [to-draw draw-game]

            [on-tick update-game 0.001]

            [on-key move-paddle]

            [stop-when out-of-bound?]))

;draw-game : game -> image

(define (draw-game g)
  (place-images
   (list BALL PADDLE (SCORE-BOX (game-score g)))
   (list (ball-loc (game-ball g)) (make-posn PADDLE-X (game-paddle g)) SCORE-POSITION)
   SCENE))


;update-game :game -> game
; update game state based on clock tick
(define (update-game g)
  (make-game (update-ball (game-ball g) (game-paddle g))
             (rationalize-paddle-pos (game-paddle g))
             (update-score g)))


; ball paddle -> ball
; update ball state, handle conditions when it hit boundaries or paddle
(define (update-ball b p)
  (cond
    [(> BALL-RADIUS (posn-x (ball-loc b))) (flip-dx b)] ;; left
    [(> BALL-RADIUS (posn-y (ball-loc b))) (flip-dy b)] ;; top
    [(< (- SCENE-HEIGHT BALL-RADIUS) (posn-y (ball-loc b))) (flip-dy b)] ;;bottom
    [(hit-paddle? b p)  (update-ball-after-hit b p)]
    [else (update-loc b)]))


; ball paddle -> ball
; update the state of ball after hitting paddle
; setting hit-wall to #false
; rationalize ball position by updating loc
(define (update-ball-after-hit b p)
  (update-loc (make-ball (ball-loc b) (react-to-paddle b p) #false)))

  
; ball -> ball
; flip the sign of the dx element in ball's velocity
; move the ball away from the boundary/paddle so that it won't keep flipping(critical distance bug.)
(define (flip-dx b)
  (update-loc (make-ball (ball-loc b)
                         (make-vector (* -1 (difficulty (ball-vel b)) (vector-x (ball-vel b)))
                                      (* (difficulty (ball-vel b)) (vector-y (ball-vel b))))
                         #true)))


; ball -> ball
; flip the sign of the dy element in ball's velocity
; move the ball away from the boundary/paddle so that it won't keep flipping(critical distance bug.)
(define (flip-dy b)
  (update-loc (make-ball (ball-loc b)
                         (make-vector (* (difficulty (ball-vel b)) (vector-x (ball-vel b)))
                                      (* -1 (difficulty (ball-vel b)) (vector-y (ball-vel b))))
                         #true)))




; ball paddle -> boolean
; determines whether the ball has hit the paddle
(define (hit-paddle? b p)
  (> (+ PADDLE-RADIUS BALL-RADIUS)
     (distance (ball-loc b) (make-posn PADDLE-X p))))


; ball -> ball
; move the ball based on its vectorocity
(define (update-loc b)
  (make-ball (make-posn (+ (posn-x (ball-loc b)) (vector-x (ball-vel b)))
                        (+ (posn-y (ball-loc b)) (vector-y (ball-vel b))))
             (ball-vel b)
             (ball-hit-wall b)))


;update-score game->score
(define (update-score g)
  (if (hit-paddle? (game-ball g) (game-paddle g))
      (+ 1 (game-score g))
      (game-score g)))


; game keyevent -> game
; move paddle based on key inputs
(define (move-paddle g ke)
  (cond
    [(string=? "up" ke) (make-game (game-ball g) (- (game-paddle g) PADDLE-SPEED) (game-score g))]
    [(string=? "down" ke) (make-game (game-ball g) (+ (game-paddle g) PADDLE-SPEED) (game-score g))]
    [else g]))


; paddle -> paddle
; move the paddle into rationalized position if it goes out of bound
(define (rationalize-paddle-pos pad)
  (cond
    [(> 0 (- pad PADDLE-RADIUS )) PADDLE-RADIUS]
    [(< SCENE-HEIGHT (+  pad PADDLE-RADIUS 2)) (- SCENE-HEIGHT PADDLE-RADIUS)]
    [else pad]))



; game -> boolean
; return true if the ball crosses right boundary
(define (out-of-bound? g)
  (if (>= SCENE-WIDTH (posn-x (ball-loc (game-ball g)))) #false #true))

(main (make-game (make-ball (make-posn PONG-INITX PONG-INITY) (make-vector 1 1) #true) 150 0))
