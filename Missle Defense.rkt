;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Missle Defense|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;Game info:
;; You will control the Bullet launcher, and by clicking your mouse on
;;any place of the canvas, you will shoot out a bullet, and your mission
;;is to prevent missiles hitting the ground. Each time one missle hits the
;;ground, you will lose 10 Hp; When Hp hits 0 game will terminate 


;;source code

;;;;****SPRITE****
;;A Sprite is a (make-sprite Posn Posn Number String)
 ;;; where
 ;;; - LOC is the sprite's location
 ;;; - VEL is its velocity
 ;;; - SIZE is the radius of the sprite
 ;;; - COLOR is its color.
 ;;; Location, velocity and size are in computer-graphic/pixel coordinates.
 ;;; A sprite represents either an attacker's missile or a defender's
 ;;; anti-missile bullet.
(define-struct sprite (loc vel size color))
#;(define (tem-sprite spr)
    ...(sprite-loc spr)...(sprite-vel spr)
    ...(sprite-size spr)...(sprite-color spr)...)
 ;;; A LOS (list of sprites) is one of:
 ;;; - empty
 ;;; - (cons Sprite LOS)
#; (define (tem-los alos)
     (cond [(empty? alos) ...alos...]
           [else...(first alos)...(rest alos)...]))
;;; A LOB (list of bullets) is one of:
 ;;; - empty
 ;;; - (cons Sprite LOS)
;;sample for testing
(define lob1
(list (make-sprite (make-posn 250 460) (make-posn 0 0)
10 'white)))
(define lob2 (list (make-sprite (make-posn 250 460)
(make-posn 1 1) 10 'white)))
;;; A LOM (list of missiles) is one of:
 ;;; - empty
 ;;; - (cons Sprite LOS)
;;sample for testing
(define lom1 (list (make-sprite (make-posn 250 0)
(make-posn 0 5) 10 'white)))
;;;;;;;;;;;;;;;;;;;;;;;seperate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; A world structure is a (make-world LOS LOS Number)
 ;;; - missiles: the missiles attacking the player
 ;;; - bullets: the bullets launched by the player trying to hit missiles
 ;;; - health: current health of the player -- game-over if health <= 0
(define-struct world (missiles bullets health))
#; (define (tem-world w)
     ....(world-missiles w)...(world-bullets w)...(world-health w)...)
;;sample for testing
(define world0 (make-world lom1 empty 100))
(define world3  (make-world empty (list (make-sprite (make-posn 120 600)
(make-posn 5 5) 10 'white)) 100))
(define world10 (make-world (list (make-sprite (make-posn 250 460)
(make-posn 5 5) 10 'white)) (list (make-sprite (make-posn 250 462)
(make-posn 5 5) 10 'white)) 0))
(define world1(make-world empty empty 100))
(define world11 (make-world empty empty 0))
;;;;;;;;;;;;;;;;;;;;;;;;;seperate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;BACKGROUND;;;;;;;;;
(define gun (place-image (rectangle 5 20 'solid 'cyan) 20 12
(place-image (circle 20 'solid 'cyan) 20 40
(rectangle 40 40 'solid 'white)))) (define background (place-image gun
 250 480 (empty-scene 500 500)))
;;;;;;;;;;;;;;;;Bullet,Missile Image;;;;;;;;;;;;;;;;
(define bullet (circle 10 'solid 'green))
(define missile (circle 5 'solid 'red))
;;;;;;;;;;;;;;;;;;;;;;;;;seperate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Draw-sprite : [Listof Sprite] type scn -> image
;draw all the given type of sprites in the los on the given screen
;code
(define (draw-sprite los type scn)
(cond [(empty? los) scn]
      [else (place-image type
(posn-x (sprite-loc (first los))) (posn-y (sprite-loc (first los)))
(draw-sprite  (rest los) type scn))]))
;;Draw-world: world-> image
;;draw the world on the canvas
;;testing
(check-expect (draw-world world0) (place-image (text  "HEALTH: 100"
 20 "black") 430 30 (place-image (circle 5 'solid 'red) 250 0  
background)))
;;code
(define (draw-world w)
(local ((define (draw-bullets lob scn) (draw-sprite lob bullet scn))
(define (draw-missiles lom scn) (draw-sprite lom missile scn)))
(place-image (text (string-append "HEALTH: " (number->string (world-health w)))
20 "black") 430 30 (draw-bullets (world-bullets w) (draw-missiles 
(world-missiles w) 
background)))))
;;;;;;;;;;;;;;;;;;;;;;;;;seperate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MOVING FUNCTIONS
;;move-los:[Listof Sprite](numberXX->number)(numberXX->number)->[Listof Sprite]
;;move the sprites of the los one time step
;code
(define (move-los los opx opy)
(cond [(empty? los) los]
      [else (cons (make-sprite 
(make-posn (opx (posn-x (sprite-loc (first los)))
                   (posn-x (sprite-vel (first los))))
           (opy (posn-y (sprite-loc (first los))) 
                   (posn-y (sprite-vel (first los)))))
           (sprite-vel (first los))
           (sprite-size (first los))
           (sprite-color (first los)))
           (move-los (rest los) opx opy))]))  
;move-world: w -> w
;update all the sprite in a world one time step
(define (move-world w)
(local ((define (move-lob lob) (move-los lob + -))
(define (move-lom lom) (move-los lom + +))) 
(make-world (move-lom (world-missiles w))
(move-lob (world-bullets w)) (world-health w))))
;;;;;;;;;;;;;;;;;;;;;;;;;seperate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;****ADD****
;;add-missiles los -> los
;;add one missile in random x to the lom
;;random things cannot be tested
;;code
(define (add-missiles lom)
(local ((define ranx (+ 5 (random 490))) (define ran5 (random 500))
(define rana (+ 1(random 2))))
(cond [(empty? lom) lom]
 [ (> 5 (length lom))
 (cons (make-sprite (make-posn ranx 0)
(make-posn (* rana (/ (- ran5 ranx) (sqrt (+ (sqr 500) (sqr (- ranx ran5))))))
(* rana (/ 500 (sqrt (+ (sqr 500) (sqr (- ranx ran5)))))))
(sprite-size (first lom))
(sprite-color (first lom))) lom)] [else lom])))
;;remove-alls: w -> w
;;remove all the dead sprites of the given world
;;testing
(define (remove-alls w)
(local ((define (move-dead-missiles lom lob)
(cond [(empty? lob) lom] [(empty? lom) empty]
[(collide? (first lom) lob) (move-dead-missiles (rest lom) lob)]
[else (cons (first lom) (move-dead-missiles (rest lom) lob))]))
(define (move-dead-bullets lob lom) (cond [(empty? lob) empty]
[(empty? lom) lob] [(or (collide? (first lob) lom)
(check-bound (first lob))) (move-dead-bullets (rest lob) lom)]
[else (cons (first lob) (move-dead-bullets (rest lob) lom))]))
(define (check-bound bullet)
(or (<= (posn-x (sprite-loc bullet)) 0) (<= (posn-y (sprite-loc bullet)) 0)
(>= (posn-x (sprite-loc bullet)) 500) (>= (posn-y (sprite-loc bullet)) 500)))
(define (collide? bullet lom) (cond [(empty? lom) false]
[else (or  (< (sqrt (+ (sqr (- (posn-x (sprite-loc (first lom))) 
(posn-x (sprite-loc bullet)))) (sqr (- (posn-y (sprite-loc (first lom)))
(posn-y (sprite-loc bullet)))))) 17)(collide? bullet (rest lom)))])))
(make-world (move-dead-missiles (world-missiles w) (world-bullets w))
(move-dead-bullets (world-bullets w) (world-missiles w))
(world-health w))))
;;; detonate-missiles: world -> world
;;; Remove missiles that landed..and decrement the player's health

;;testing
(check-expect (detonate world1) world1)
;;the rest of the function cannot be tested,since there is random stuff in it
;;code
(define (detonate w)
(local ((define (add-missile missile w)
  (make-world
   (cons missile
         (world-missiles w))
   (world-bullets w)
   (world-health w)))
        (define (check-land m)
(>= (posn-y (sprite-loc m)) 499)))  
  (cond [(empty? (world-missiles w)) w]
[(check-land (first (world-missiles w)))
 (detonate (make-world (rest (world-missiles w))
 (world-bullets w) (- (world-health w) 10)))]
[else (add-missile (first (world-missiles w))
 (detonate (make-world (rest (world-missiles w))
(world-bullets w) (world-health w))))])))
;;;;;;;;;;;;;;;;;;;;;WORLD TICK;;;;;;;;;;;;;;;;;;;;;;;;
;;update-world: world -> world
;;step the world one tick
;;testing
;;cannot be tested 
;;code
(define (update-world w)
(remove-alls (detonate (move-world (make-world 
(add-missiles (world-missiles w)) (world-bullets w)
(world-health w))))))
;;;;;;;;;;;;;;;MOUSE EVENT;;;;;;;;;;;;;;;;;
;;mouse-event: world x y mouse-event -> world
;;give the bullets in the world a velocity
;;testing
(check-expect (mouse-event world3 100 100 "botton-up") (make-world
 empty (list (make-sprite (make-posn 120 600) (make-posn 5 5) 10 'white)) 100)) 
;;code
(define (mouse-event w x y my-mouse)
(cond [(string=? my-mouse "button-up")
(make-world (world-missiles w) (reverse(cons (make-sprite (make-posn 250 460)
  (make-posn (* 20 (/ (- x 250) (sqrt (+ (sqr (- x 250)) (sqr (- 460 y))))))
             (* 20 (/ (- 460 y) (sqrt (+ (sqr (- x 250)) (sqr (- 460 y)))))))
  10 'red) (world-bullets w))) (world-health w))]   [else w]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameover Functions
;; stop? : World -> boolean
;; consume world and determine whether
;; the game is end
;;testing
(check-expect (stop? world0) false)
(check-expect (stop? world10) true)
(define (stop? w) (= (world-health w) 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stop scene
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;STOP:world->image
;;draw "GAME OVER" when the health equals to zero
;;testing
(check-expect (STOP world11) (place-image (text "HEALTH: 0"
20 "black") 430 30 (place-image (text "GAME OVER" 60 'black)
250 250 background)))
;;code
(define (STOP w) (place-image (text "GAME OVER" 60 'black)
250 250 (draw-world w)))
;;;;;;;;BIG BANG
(big-bang world0 (to-draw draw-world) (on-tick update-world .05)
(on-mouse mouse-event) (stop-when stop? STOP))




