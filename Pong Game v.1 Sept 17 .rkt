;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Pong Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; TOOD:
;; 1. parameters improvement
;; 2. HIT-RANGE adjustment.. debug
;; 3. Increments the velocity whenever it hits...
;; 4. nothing goes out of bound
;; 5. show score tab




(require 2htdp/universe)
(require 2htdp/image)
(define INIT-X 400)
(define INIT-Y 200)
(define WIDTH 400)
(define HEIGHT 400)
(define SCENE (rectangle WIDTH HEIGHT "outline" "red"))
(define PADDLE-SPEED 20)
(define BALL-RADIUS 10)
(define BALL (circle BALL-RADIUS "solid" "blue"))
(define PADDLE (rectangle 3 20 "solid" "red"))
(define PADDLE-X 390)
(define HIT-RANGE 11)
(define (score-box score)
  (overlay
   (text (number->string score) 20 "black")
   (rectangle 40 25 "solid" "grey")))

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

            [on-tick update-game]

            [on-key move-paddle]

            [stop-when out-of-bound?]))

;draw-game : game -> image

(define (draw-game g)
  (place-images
   (list BALL PADDLE)
   (list (ball-loc (game-ball g)) (make-posn PADDLE-X (game-paddle g)))
   SCENE))

;update-game :game -> game
; update game state based on clock tick
(define (update-game g)
  (make-game (update-ball (game-ball g) (game-paddle g))
             (game-paddle g)
             (update-score g)))

; ball paddle -> ball
; update ball state, handle conditions when it hit boundaries or paddle
(define (update-ball b p)
  (cond
    [(> BALL-RADIUS (posn-x (ball-loc b))) (flip-dx b)] ;; left
    [(> BALL-RADIUS (posn-y (ball-loc b))) (flip-dy b)] ;; top
    [(< (- HEIGHT BALL-RADIUS) (posn-y (ball-loc b))) (flip-dy b)] ;;bottom
    [(hit-paddle? b p) (flip-dx b)]
    [else (update-loc b)]))


    
; ball -> ball
; flip the sign of the dx element in ball's velocity
(define (flip-dx b)
  (make-ball (make-posn (- (posn-x (ball-loc b)) (vel-dx (ball-vel b)))
                        (+ (posn-y (ball-loc b)) (vel-dy (ball-vel b))))
             (make-vel (* -1 (vel-dx (ball-vel b))) (vel-dy (ball-vel b)))))

; ball -> ball
; flip the sign of the dy element in ball's velocity
(define (flip-dy b)
  (make-ball (make-posn (+ (posn-x (ball-loc b)) (vel-dx (ball-vel b)))
                        (- (posn-y (ball-loc b)) (vel-dy (ball-vel b))))
             (make-vel (vel-dx (ball-vel b)) (* -1 (vel-dy (ball-vel b))))))


; ball paddle -> boolean
; determines whether the ball has hit the paddle
(define (hit-paddle? b p)
  (and (> HIT-RANGE (- PADDLE-X (posn-x (ball-loc b))))
       (> HIT-RANGE (abs (- p (posn-y (ball-loc b)))))))


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


; game -> boolean
; return true if the ball crosses right boundary
(define (out-of-bound? g)
  (if (>= WIDTH (posn-x (ball-loc (game-ball g)))) #false #true))

(main (make-game (make-ball (make-posn 200 200) (make-vel 1 1)) 50 0))
