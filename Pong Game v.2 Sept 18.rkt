;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pong Game v.2 Sept 18|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; TOOD:
;; 1. parameters improvement //NAH
;; 2. HIT-RANGE adjustment.. debug // MAKE HIT-RANGE 
;; 3. Increments the velocity whenever it hits... //FINISHED? addition or exponential
;; 4. nothing goes out of bound  // add condition to movement of paddle
;; 5. Improve flipping function
;; 6. Semi-circle Paddle Model, functions
;; 7. boolean function? -->> disable ke when at top/bottom rationalized position
;; 8. error handlings.
;; 9. score is set to paddle position



;; 5. score //FINISHED

(require 2htdp/universe)
(require 2htdp/image)

(define PONG-INITX 250)
(define PONG-INITY 200)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 400)
(define SCENE (rectangle SCENE-WIDTH SCENE-HEIGHT "outline" "red"))


(define BALL-RADIUS 5)
(define BALL (circle BALL-RADIUS "solid" "blue"))

(define PADDLE-SPEED 20)
(define PADDLE-WIDTH 3)
(define PADDLE-HEIGHT 20)
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" "red"))
(define PADDLE-X 390)

(define HIT-RANGE-PARA 0)
(define HIT-RANGE-X (+ HIT-RANGE-PARA BALL-RADIUS (/ PADDLE-WIDTH 2)))
(define HIT-RANGE-Y (+ BALL-RADIUS (/ PADDLE-HEIGHT 2)))

(define DIFFICULTY 1.1)

(define (SCORE-BOX score)
  (overlay
   (text (number->string score) 20 "black")
   (rectangle 40 25 "solid" "white")))
(define SCORE-POSITION (make-posn 50 50))

(define-struct vel [dx dy])
(define-struct ball [loc vel])
;; loc is a (make-posn NNN NN)
;; NNN is a non-negative number

(define-struct game [ball paddle score])
;; a paddle is a NNN, the paddle's y-coord
;; a score is the NNN

(define (main game)
  
  (big-bang game

            [to-draw draw-game]

            [on-tick update-game 0.01]

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
    [(hit-paddle? b p) (flip-dx b)]
    [else (update-loc b)]))



; ball -> ball
; flip the sign of the dx element in ball's velocity
; move the ball away from the boundary/paddle so that it won't keep flipping(critical distance bug.)
(define (flip-dx b)
  (update-loc (make-ball (ball-loc b)
                         (make-vel (* -1 DIFFICULTY (vel-dx (ball-vel b))) (* DIFFICULTY (vel-dy (ball-vel b)))))))


; ball -> ball
; flip the sign of the dy element in ball's velocity
; move the ball away from the boundary/paddle so that it won't keep flipping(critical distance bug.)
(define (flip-dy b)
  (update-loc (make-ball (ball-loc b)
                         (make-vel (* DIFFICULTY (vel-dx (ball-vel b))) (* -1 DIFFICULTY (vel-dy (ball-vel b)))))))



; ball paddle -> boolean
; determines whether the ball has hit the paddle
(define (hit-paddle? b p)
  (and (> HIT-RANGE-X (- PADDLE-X (posn-x (ball-loc b))))
       (> HIT-RANGE-Y (abs (- p (posn-y (ball-loc b)))))))


; ball -> ball
; move the ball based on its velocity
(define (update-loc b)
  (make-ball (make-posn (+ (posn-x (ball-loc b)) (vel-dx (ball-vel b)))
                        (+ (posn-y (ball-loc b)) (vel-dy (ball-vel b))))
             (ball-vel b)))

;update-score game->score
(define (update-score g)
  (if (hit-paddle? (game-ball g) (game-paddle g))
      (+ 1 (game-paddle g))
      (game-paddle g)))


; game keyevent -> game
; move paddle based on key inputs
(define (move-paddle g ke)
  (cond
    [(string=? "up" ke) (make-game (game-ball g) (- (game-paddle g) PADDLE-SPEED) (game-score g))]
    [(string=? "down" ke) (make-game (game-ball g) (+ (game-paddle g) PADDLE-SPEED) (game-score g))]
    [else g]))

; paddle -> paddle
; ratioanlize the position of paddle if it goes out of either edge
(define (rationalize-paddle-pos pad)
  (cond
    [(> 0 (- pad (/ PADDLE-HEIGHT 2))) (/ PADDLE-HEIGHT 2)]
    [(< SCENE-HEIGHT (+  pad (/ PADDLE-HEIGHT 2))) (- SCENE-HEIGHT (/ PADDLE-HEIGHT 2))]
    [else pad]))


; game -> boolean
; return true if the ball crosses right boundary
(define (out-of-bound? g)
  (if (>= SCENE-WIDTH (posn-x (ball-loc (game-ball g)))) #false #true))

(main (make-game (make-ball (make-posn PONG-INITX PONG-INITY) (make-vel -0.7 0.7)) 190 0))
