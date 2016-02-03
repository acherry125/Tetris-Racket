;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |FULL TETRIS|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Definitions

;;; A Block is a (make-block Number Number Color)
;;; Interpretation: x and y are the position of the block in grid coordinates
;;; and color is the color of the block
(define-struct block (x y color))

;;; A Tetra is a (make-tetra Posn BSet)
;;; The center point is the point around which the tetra rotates
;;; when it spins. 
(define-struct tetra (center blocks))

;;; A Set of Blocks (BSet) is one of:
;;; - empty
;;; - (cons Block BSet)
;;; Order does not matter.

;;; A World is a (make-world Tetra BSet Number)
;;; The BSet represents the pile of blocks at the bottom of the screen, 
;; the tetra is the playable tetra, the score is the number blocks in BSet
(define-struct world (tetra pile score))

;;; A Direction is one of:
;;; - "left"     
;;; - "right"

;;; Constants
(define PIXELS/GRID 10)
(define BOARD-HEIGHT 20) ; in grid cells
(define BOARD-WIDTH 10)  ; in grid cells
(define BOARD (empty-scene (* BOARD-WIDTH PIXELS/GRID) (* BOARD-HEIGHT PIXELS/GRID)))

(define DEFAULT-SPEED 1) ; in grid cells 
(define FAST-SPEED 3)  ; in grid cells 

(define OUTLINE (square 10 "outline" 'black))

(define END-TETRA (make-tetra (make-posn 0 0) empty))
(define O-START (make-tetra (make-posn 50 10)  
                            (list (make-block 4 0 'limegreen) 
                                  (make-block 5 0 'limegreen)
                                  (make-block 4 1 'limegreen)
                                  (make-block 5 1 'limegreen))))

(define I-START (make-tetra (make-posn 4 0) 
                            (list (make-block 3 0 'blue) 
                                  (make-block 4 0 'blue)
                                  (make-block 5 0 'blue)
                                  (make-block 6 0 'blue))))

(define L-START (make-tetra (make-posn 5 1) 
                            (list (make-block 4 1 'purple) 
                                  (make-block 5 1 'purple)
                                  (make-block 6 1 'purple)
                                  (make-block 6 0 'purple))))

(define J-START (make-tetra (make-posn 5 1) 
                            (list (make-block 4 0 'cyan) 
                                  (make-block 4 1 'cyan)
                                  (make-block 5 1 'cyan)
                                  (make-block 6 1 'cyan))))

(define T-START (make-tetra (make-posn 4 1) 
                            (list (make-block 3 1 'orange) 
                                  (make-block 4 1 'orange)
                                  (make-block 4 0 'orange)
                                  (make-block 5 1 'orange))))

(define Z-START (make-tetra (make-posn 4 0) 
                            (list (make-block 3 0 'pink) 
                                  (make-block 4 0 'pink)
                                  (make-block 4 1 'pink)
                                  (make-block 5 1 'pink))))

(define S-START (make-tetra (make-posn 4 0) 
                            (list (make-block 3 1 'red) 
                                  (make-block 4 1 'red)
                                  (make-block 4 0 'red)
                                  (make-block 5 0 'red))))

(define INITIAL-WORLD (make-world S-START empty 0))

(define TEST-WORLD (make-world S-START 
                               (list (make-block 3 18 'limegreen) 
                                     (make-block 4 18 'limegreen)
                                     (make-block 5 18 'limegreen)
                                     (make-block 6 18 'limegreen))
                               500))

(define FAIL-WORLD (make-world S-START 
                               (list (make-block 3 0 'limegreen) 
                                     (make-block 4 18 'limegreen)
                                     (make-block 5 18 'limegreen)
                                     (make-block 6 18 'limegreen))
                               4))
(define COLLIDED-WORLD (make-world (make-tetra (make-posn 4 0) 
                                               (list (make-block 3 1 'red) 
                                                     (make-block 4 1 'red)
                                                     (make-block 4 0 'red)
                                                     (make-block 5 0 'red)))
                                   (list (make-block 3 2 'limegreen)
                                         (make-block 4 2 'limegreen)
                                         (make-block 5 2 'limegreen)
                                         (make-block 6 2 'limegreen)) 20))
;;------------------------------------------------------------------------------
;;; Render System ;;;

;;; place-image/grid : Image Number Number Image -> Image
;;; Places an image at the given grid coordinates on a board.
;;; Restrictions: the two grid coordinates (Number Number) must be positive
(check-expect (place-image/grid (square 10 "solid" 'red) 5 5 (empty-scene 100 100))
              (place-image (square 10 "solid" 'red) 55 55 (empty-scene 100 100)))
(check-expect (place-image/grid (square 10 "solid" 'red) 9 2 (empty-scene 100 100))
              (place-image (square 10 "solid" 'red) 95 25 (empty-scene 100 100)))
(check-expect (place-image/grid (square 10 "solid" 'red) 2 7 (empty-scene 100 100))
              (place-image (square 10 "solid" 'red) 25 75 (empty-scene 100 100)))

(define (place-image/grid image x y scene)
  (place-image image (* (+ x .5) PIXELS/GRID) (* (+ y .5) PIXELS/GRID) scene))

;;; place-tetra : Tetra Image -> Image
;;; Places a Tetra on the board.
(check-expect 
 (place-tetra END-TETRA BOARD) BOARD)

(check-expect 
 (place-tetra S-START BOARD)
 (place-image/grid (square 10 "outline" 'black) 3 1
                   (place-image/grid 
                    (square 10 "solid" 'red) 3 1
                    (place-image/grid 
                     (square 10 "outline" 'black) 4 1
                     (place-image/grid 
                      (square 10 "solid" 'red) 4 1 
                      (place-image/grid 
                       (square 10 "outline" 'black) 4 0
                       (place-image/grid 
                        (square 10 "solid" 'red) 4 0
                        (place-image/grid 
                         (square 10 "outline" 'black) 5 0
                         (place-image/grid 
                          (square 10 "solid" 'red) 5 0
                          BOARD)))))))))

(check-expect 
 (place-tetra T-START BOARD)
 (place-image/grid (square 10 "outline" 'black) 3 1
                   (place-image/grid 
                    (square 10 "solid" 'orange) 3 1
                    (place-image/grid 
                     (square 10 "outline" 'black) 4 1
                     (place-image/grid 
                      (square 10 "solid" 'orange) 4 1 
                      (place-image/grid 
                       (square 10 "outline" 'black) 4 0
                       (place-image/grid 
                        (square 10 "solid" 'orange) 4 0
                        (place-image/grid 
                         (square 10 "outline" 'black) 5 1
                         (place-image/grid 
                          (square 10 "solid" 'orange) 5 1
                          BOARD)))))))))

(define (place-tetra tetra image)
  (place-bset (tetra-blocks tetra) image))

;;; place-bset : BSet Image -> Image
;;; Places the BSet on the Image
;;; (square 10 "solid" (block-color (fourth (tetra-blocks tetra))))
(check-expect 
 (place-bset empty BOARD) BOARD)

(check-expect 
 (place-bset (list (make-block 4 0 'limegreen) 
                   (make-block 5 0 'limegreen)
                   (make-block 4 1 'limegreen)
                   (make-block 5 1 'limegreen)) 
             BOARD)
 (place-image/grid (square 10 "outline" 'black) 4 0
                   (place-image/grid 
                    (square 10 "solid" 'limegreen) 4 0
                    (place-image/grid 
                     (square 10 "outline" 'black) 5 0
                     (place-image/grid 
                      (square 10 "solid" 'limegreen) 5 0
                      (place-image/grid 
                       (square 10 "outline" 'black) 4 1
                       (place-image/grid 
                        (square 10 "solid" 'limegreen) 4 1
                        (place-image/grid 
                         (square 10 "outline" 'black) 5 1
                         (place-image/grid 
                          (square 10 "solid" 'limegreen) 5 1
                          BOARD)))))))))

(define (place-bset bset image)
  (cond [(empty? bset) image]
        [else (place-image/grid OUTLINE
                                (block-x (first bset))
                                (block-y (first bset))  
                                (place-image/grid (square 10 "solid" (block-color (first bset))) 
                                                  (block-x (first bset))
                                                  (block-y (first bset))
                                                  (place-bset (rest bset) image)))]))

;;; place-score : Number Image -> Image 
;;; Displays the Score on the Image
(check-expect (place-score 500 BOARD) 
              (place-image/grid (text "500" 20 'black) 7 1 BOARD))
(check-expect (place-score 1500 BOARD) 
              (place-image/grid (text "1500" 20 'black) 7 1 BOARD))

(define (place-score score image)
  (place-image/grid (text (number->string score) 20 'black) 7 1 image))

;;; display-end : World -> Image
;;; Displays the final score over the final world state
(check-expect (display-end FAIL-WORLD) 
              (place-image/grid 
               (text "Press R to Continue" 10 'black) 4.5 18
               (place-image/grid 
                (text "SCORE" 25 'black) 4.5 7
                (place-image/grid 
                 (text "4" 45 'black) 4.5 10 
                 (place-bset 
                  (list (make-block 3 0 'limegreen) 
                        (make-block 4 18 'limegreen)
                        (make-block 5 18 'limegreen)
                        (make-block 6 18 'limegreen)) BOARD)))))

(define (display-end world)
  (place-image/grid 
   (text "Press R to Continue" 10 'black) 4.5 18
   (place-image/grid (text "SCORE" 25 'black) 4.5 7
                     (place-image/grid 
                      (text (number->string (world-score world)) 45 'black) 4.5 10
                      (place-bset (world-pile world) BOARD)))))

;;; lose? : BSeT -> Boolean
;;; Determines if the BSeT has reached the top of the screen (fail state)
(check-expect (lose? empty) false)
(check-expect (lose? (world-pile FAIL-WORLD)) true)
(check-expect (lose? (world-pile TEST-WORLD)) false)

(define (lose? bset)
  (cond [(empty? bset) false]
        [else (or (= (block-y (first bset)) 0) (lose? (rest bset)))]))

;;; draw-board : World -> Image 
;;; Renders the world
(check-expect 
 (draw-board (make-world S-START 
                         (list (make-block 3 18 'limegreen) 
                               (make-block 4 18 'limegreen)
                               (make-block 5 18 'limegreen)
                               (make-block 6 18 'limegreen))
                         500)) 
 (place-score 500 
              (place-tetra S-START 
                           (place-bset 
                            (list (make-block 3 18 'limegreen) 
                                  (make-block 4 18 'limegreen)
                                  (make-block 5 18 'limegreen)
                                  (make-block 6 18 'limegreen))
                            BOARD))))
(check-expect
 (draw-board FAIL-WORLD)
 (place-image/grid (text "Press R to Continue" 10 'black) 4.5 18
                   (place-image/grid (text "SCORE" 25 'black) 4.5 7
                                     (place-image/grid 
                                      (text "4" 45 'black) 4.5 10 
                                      (place-bset 
                                       (list (make-block 3 0 'limegreen) 
                                             (make-block 4 18 'limegreen)
                                             (make-block 5 18 'limegreen)
                                             (make-block 6 18 'limegreen)) BOARD)))))


(define (draw-board world)
  (cond [(lose? (world-pile world)) (display-end world)]
        [else
         (place-score (world-score world) 
                      (place-tetra (world-tetra world) 
                                   (place-bset (world-pile world) BOARD)))])) 

;;------------------------------------------------------------------------------
;;; World Update Functions ;;;

;;; update-tetra : Tetra -> Tetra
;;; Moves the tetra one down per tick
(check-expect (update-tetra S-START DEFAULT-SPEED)
              (make-tetra (make-posn 4 1) 
                          (list (make-block 3 2 'red) 
                                (make-block 4 2 'red)
                                (make-block 4 1 'red)
                                (make-block 5 1 'red))))

(define (update-tetra tetra speed)
  (make-tetra (make-posn (posn-x (tetra-center tetra))
                         (+ (posn-y (tetra-center tetra)) speed))
              (add-to-bset-y (tetra-blocks tetra) speed)))

;;; add-to-bset-y : BSet Number -> BSet
;;; Adds a given number to the y of every block in the BSet.
(check-expect (add-to-bset-y empty 55) empty)

(check-expect (add-to-bset-y (list (make-block 3 2 'red) 
                                   (make-block 4 2 'red)
                                   (make-block 4 1 'red)
                                   (make-block 5 1 'red)) 1)
              (list (make-block 3 3 'red) 
                    (make-block 4 3 'red)
                    (make-block 4 2 'red)
                    (make-block 5 2 'red)))

(check-expect (add-to-bset-y (list (make-block 3 2 'red) 
                                   (make-block 4 2 'red)) 30)
              (list (make-block 3 32 'red) 
                    (make-block 4 32 'red)))

(define (add-to-bset-y a-bset add-y)
  (cond
    [(empty? a-bset) empty]
    [else (cons 
           (update-block-pos (first a-bset) 
                             (block-x (first a-bset))
                             (+ (block-y (first a-bset)) add-y))
           (add-to-bset-y (rest a-bset) add-y))]))
;;; add-to-tetra-x : Tetra Number -> Tetra
;;; Adds the given number to the tetra's x positions
(check-expect (add-to-tetra-x S-START 1)
              (make-tetra (make-posn 5 0) 
                          (list (make-block 4 1 'red) 
                                (make-block 5 1 'red)
                                (make-block 5 0 'red)
                                (make-block 6 0 'red))))

(check-expect (add-to-tetra-x S-START -1)
              (make-tetra (make-posn 3 0) 
                          (list (make-block 2 1 'red) 
                                (make-block 3 1 'red)
                                (make-block 3 0 'red)
                                (make-block 4 0 'red))))

(define (add-to-tetra-x tetra add-x)
  (make-tetra (make-posn (+ add-x (posn-x (tetra-center tetra)))
                         (posn-y (tetra-center tetra)))
              (add-to-bset-x (tetra-blocks tetra) add-x)))

;;; add-to-bset-x : BSet Number -> BSet
;;; Adds a given number to the x of every block in the BSet.
(check-expect (add-to-bset-x empty 55) empty)

(check-expect (add-to-bset-x (list (make-block 3 2 'red) 
                                   (make-block 4 2 'red)
                                   (make-block 4 1 'red)
                                   (make-block 5 1 'red)) 1)
              (list (make-block 4 2 'red) 
                    (make-block 5 2 'red)
                    (make-block 5 1 'red)
                    (make-block 6 1 'red)))

(check-expect (add-to-bset-x (list (make-block 3 2 'red) 
                                   (make-block 4 2 'red)) 30)
              (list (make-block 33 2 'red) 
                    (make-block 34 2 'red)))

(define (add-to-bset-x a-bset add-x)
  (cond
    [(empty? a-bset) empty]
    [else (cons 
           (update-block-pos (first a-bset) 
                             (+ (block-x (first a-bset)) add-x)
                             (block-y (first a-bset)))
           (add-to-bset-x (rest a-bset) add-x))]))

;;; update-block-pos : Block Number Number -> Block
;;; Updates the block's position with the new x and y
(check-expect (update-block-pos (make-block 14 15 'blue) 12 20)
              (make-block 12 20 'blue))

(check-expect (update-block-pos (make-block 10 50 'green) 5 2)
              (make-block 5 2 'green))

(define (update-block-pos block new-x new-y)
  (make-block new-x new-y (block-color block)))

;;; new-tetra : Anything -> Tetra
;;; Returns new random tetra placed at the top of the world. Ignores input
(check-random (new-tetra 5) (get-tetra (random 7)))

(define (new-tetra ignore)
  (get-tetra (random 7)))

;;; get-tetra : Number -> Tetra
;;; Returns a tetra corresponding to the given number.
;;; Tetra number combinations: 0, "O";
;;; 1, "I"; 2, "L"; 3, "J"; 4, "T"; 5, "Z"; 6, "S"
(check-expect (get-tetra 0) O-START)
(check-expect (get-tetra 1) I-START)
(check-expect (get-tetra 2) L-START)
(check-expect (get-tetra 3) J-START)
(check-expect (get-tetra 4) T-START)
(check-expect (get-tetra 5) Z-START)
(check-expect (get-tetra 6) S-START)

(define (get-tetra tetra-num)
  (cond [(= 0 tetra-num) O-START]
        [(= 1 tetra-num) I-START]
        [(= 2 tetra-num) L-START]
        [(= 3 tetra-num) J-START]
        [(= 4 tetra-num) T-START]
        [(= 5 tetra-num) Z-START]
        [(= 6 tetra-num) S-START]))

;;; update-score : BSet -> Number
;;; Updates the score based on the number of blocks in the BSet.
(check-expect (update-score empty) 0)

(check-expect (update-score (list (make-block 4 4 'red)
                                  (make-block 2 2 'blue)
                                  (make-block 30 2 'green))) 3)
(define (update-score a-bset)
  (cond
    [(empty? a-bset) 0]
    [else (+ 1 (update-score (rest a-bset)))]))

;;; move-to-pile : Tetra BSet -> BSet
;;; Adds current tetra's block set to BSet.
(check-expect (move-to-pile (make-tetra (make-posn 4 1)
                                        (list (make-block 3 2 'red))) empty)
              (list (make-block 3 2 'red)))

(check-expect (move-to-pile (make-tetra (make-posn 4 1) 
                                        (list (make-block 3 2 'red) 
                                              (make-block 4 2 'red)
                                              (make-block 4 1 'red)
                                              (make-block 5 1 'red)))
                            (list (make-block 3 18 'limegreen) 
                                  (make-block 4 18 'limegreen)
                                  (make-block 5 18 'limegreen)
                                  (make-block 6 18 'limegreen)))
              (list (make-block 3 2 'red) 
                    (make-block 4 2 'red)
                    (make-block 4 1 'red)
                    (make-block 5 1 'red) 
                    (make-block 3 18 'limegreen) 
                    (make-block 4 18 'limegreen)
                    (make-block 5 18 'limegreen)
                    (make-block 6 18 'limegreen)
                    ))

(define (move-to-pile tetra bset)
  (append-bset (tetra-blocks tetra) bset))

;;; append-bset : BSet BSet -> BSet
;;; Appends the content of the first BSet onto the begining of the second.
(check-expect (append-bset (list (make-block 4 4 'green)) empty)
              (list (make-block 4 4 'green)))
(check-expect (append-bset empty (list (make-block 2 2 'blue)))
              (list (make-block 2 2 'blue)))
(check-expect (append-bset (list (make-block 3 2 'red) 
                                 (make-block 4 2 'red)
                                 (make-block 4 1 'red)
                                 (make-block 5 1 'red))
                           (list (make-block 3 18 'limegreen) 
                                 (make-block 4 18 'limegreen)
                                 (make-block 5 18 'limegreen)
                                 (make-block 6 18 'limegreen)))
              (list
               (make-block 3 2 'red) 
               (make-block 4 2 'red)
               (make-block 4 1 'red)
               (make-block 5 1 'red)
               (make-block 3 18 'limegreen) 
               (make-block 4 18 'limegreen)
               (make-block 5 18 'limegreen)
               (make-block 6 18 'limegreen)
               ))

(define (append-bset bset1 bset2)
  (cond
    [(empty? bset1) bset2]
    [(empty? bset2) bset1]
    [else (cons (first bset1) (append-bset (rest bset1) bset2))]))

;;; Collision Detection
;;; collide-y? : World -> Boolean
;;; Checks to see if the bottom of the tetra has landed
;;; on the ground or the pile
(check-expect (collide-y? TEST-WORLD) false)

(check-expect (collide-y? COLLIDED-WORLD) true)

(check-expect (collide-y? (make-world 
                           (make-tetra 
                            (make-posn 5 5)
                            (list 
                             (make-block 19 19 'green)
                             (make-block 18 19 'green))) empty 0)) true)

(check-expect (collide-y? (make-world 
                           (make-tetra 
                            (make-posn 5 5)
                            (list 
                             (make-block 19 18 'green)
                             (make-block 18 18 'green))) empty 0)) false)

(define (collide-y? world)
  (or (bset-adjacent-y? (tetra-blocks (world-tetra world)) (world-pile world))
      (collide-with-floor? (tetra-blocks (world-tetra world)))))

;;; bset-adjacent-y? : BSet Bset -> Boolean
;;; Checks if any of the blocks in the two bsets are adjacent to each other,
;;; bottom to top.
(check-expect (bset-adjacent-y? empty 
                                (list (make-block 4 6 'blue))) false)

(check-expect (bset-adjacent-y? (list (make-block 4 4 'blue)) 
                                (list (make-block 4 6 'blue))) false)

(check-expect (bset-adjacent-y? (list (make-block 4 4 'blue)) 
                                (list (make-block 4 5 'blue))) true)

(check-expect (bset-adjacent-y? (list (make-block 4 4 'blue)
                                      (make-block 5 4 'blue)) 
                                (list (make-block 5 5 'blue)
                                      (make-block 6 5 'blue))) true)

(check-expect (bset-adjacent-y? (list (make-block 4 4 'blue)) 
                                (list (make-block 5 5 'blue))) false)

(define (bset-adjacent-y? bset1 bset2)
  (cond
    [(empty? bset1) false]
    [else (or (block-adjacent-to-bset-y? (first bset1) bset2)
              (bset-adjacent-y? (rest bset1) bset2))]))

;;; block-adjacent-to-bset-y? : Block BSet -> Boolean
;;; Checks if the block is adjacent, top to bottom, 
;;; to any of the blocks in the BSet.
(check-expect (block-adjacent-to-bset-y? (make-block 4 6 'red) empty) false)

(check-expect (block-adjacent-to-bset-y?
               (make-block 5 4 'blue) 
               (list (make-block 5 5 'blue)
                     (make-block 6 5 'blue))) true)

(check-expect (block-adjacent-to-bset-y?
               (make-block 6 4 'blue) 
               (list (make-block 5 5 'blue)
                     (make-block 6 5 'blue))) true)

(check-expect (block-adjacent-to-bset-y?
               (make-block 4 4 'blue) 
               (list (make-block 5 5 'blue)
                     (make-block 6 5 'blue))) false)

(define (block-adjacent-to-bset-y? block bset)
  (cond
    [(empty? bset) false]
    [else (or (block-adjacent-y? block (first bset))
              (block-adjacent-to-bset-y? block (rest bset)))]))

;;; block-adjacent-y? : Block Block -> Boolean
;;; Checks to see if to blocks are adjacent top to bottom.

(check-expect (block-adjacent-y?
               (make-block 5 4 'blue) 
               (make-block 5 5 'blue)) true)

(check-expect (block-adjacent-y?
               (make-block 4 4 'blue) 
               (make-block 5 5 'blue)) false)

(check-expect (block-adjacent-y?
               (make-block 4 4 'blue)
               (make-block 5 4 'blue)) false)

(define (block-adjacent-y? block1 block2)
  (and (= (- (block-y block2) (block-y block1)) 1)
       (= (block-x block2) (block-x block1))))

;;; collide-with-floor? : BSet -> Boolean
;;; Returns if any of the blocks has collided with the floor.
(check-expect (collide-with-floor? empty) false)

(check-expect (collide-with-floor? (list 
                                    (make-block 19 18 'green)
                                    (make-block 18 18 'green))) false)

(check-expect (collide-with-floor? (list 
                                    (make-block 19 19 'green)
                                    (make-block 18 19 'green))) true)

(define (collide-with-floor? bset)
  (cond
    [(empty? bset) false]
    [else (or (block-on-floor? (first bset))
              (collide-with-floor? (rest bset)))]))

;;; block-on-floor? : Block -> Boolean
;;; Checks if the block is on the floor
(check-expect (block-on-floor? (make-block 19 18 'blue)) false)

(check-expect (block-on-floor? (make-block 19 19 'blue)) true)

(define (block-on-floor? block)
  (= (+ (block-y block) 1) BOARD-HEIGHT)) 

;;; next-world : World -> World
;;; Updates the world
(check-expect (next-world FAIL-WORLD) FAIL-WORLD)

(check-expect (next-world TEST-WORLD) 
              (make-world  (make-tetra (make-posn 4 1)
                                       (list (make-block 3 2 'red) 
                                             (make-block 4 2 'red)
                                             (make-block 4 1 'red)
                                             (make-block 5 1 'red)))
                           (list (make-block 3 18 'limegreen) 
                                 (make-block 4 18 'limegreen)
                                 (make-block 5 18 'limegreen)
                                 (make-block 6 18 'limegreen))
                           4))

(check-random (next-world COLLIDED-WORLD) 
              (make-world  (new-tetra 4)
                           (list
                            (make-block 3 1 'red) 
                            (make-block 4 1 'red)
                            (make-block 4 0 'red)
                            (make-block 5 0 'red)
                            (make-block 3 2 'limegreen) 
                            (make-block 4 2 'limegreen)
                            (make-block 5 2 'limegreen)
                            (make-block 6 2 'limegreen))
                           4))

(define (next-world w)
  (cond
    [(lose? (world-pile w)) (make-world (world-tetra w)
                                        (world-pile w)
                                        (update-score (world-pile w)))]
    [(collide-y? w) (make-world (new-tetra 4) 
                                (move-to-pile
                                 (world-tetra w)
                                 (world-pile w))
                                (update-score (world-pile w)))]
    [else (make-world (update-tetra (world-tetra w) DEFAULT-SPEED) 
                      (world-pile w) 
                      (update-score (world-pile w)))]))

;;------------------------------------------------------------------------------

;;; Key Handler Code ;;;
;;; key-handler : World Key -> World
;;; Moves the block appropriately according to the pressed key
(check-expect (key-handler INITIAL-WORLD "right")
              (make-world (make-tetra 
                           (make-posn 5 0)
                           (list (make-block 4 1 'red)
                                 (make-block 5 1 'red)
                                 (make-block 5 0 'red)
                                 (make-block 6 0 'red)))
                          empty 0))

(check-expect (key-handler INITIAL-WORLD "left")
              (make-world (make-tetra 
                           (make-posn 3 0)
                           (list (make-block 2 1 'red)
                                 (make-block 3 1 'red)
                                 (make-block 3 0 'red)
                                 (make-block 4 0 'red)))
                          empty 0))

(check-expect (key-handler (make-world (make-tetra (make-posn 5 5)
                                                   (list (make-block 6 5 'red)
                                                         (make-block 5 6 'red)
                                                         (make-block 4 5 'red))) empty 0)                                      
                           "a")
              (make-world (make-tetra 
                           (make-posn 5 5)
                           (list (make-block 5 4 'red)
                                 (make-block 6 5 'red)
                                 (make-block 5 6 'red)))                          
                          empty 0))

(check-expect (key-handler (make-world (make-tetra (make-posn 5 5)
                                                   (list (make-block 6 5 'red)
                                                         (make-block 5 6 'red)
                                                         (make-block 4 5 'red))) empty 0)                                      
                           "s")
              (make-world (make-tetra 
                           (make-posn 5 5)
                           (list (make-block 5 6 'red)
                                 (make-block 4 5 'red)
                                 (make-block 5 4 'red)))                          
                          empty 0))

(check-expect (key-handler TEST-WORLD "r")
              INITIAL-WORLD)

(define (key-handler w key)
  (cond [(key=? "r" key) INITIAL-WORLD] 
        [(key=? "left" key) (move-sideways 'left w)]
        [(key=? "right" key) (move-sideways 'right w)]
        [(key=? "a" key) (tetra-rotate 'left w)]
        [(key=? "s" key) (tetra-rotate 'right w)]
        [else w]
        ))

;;; fall-faster:
;;; Makes the tetra fall faster
;;; TO-DO: Make this function

;;; tetra-rotate : Direction World -> World
;;; Rotates the tetra in a given direction
(check-expect (tetra-rotate 'right 
                            (make-world (make-tetra (make-posn 5 5)
                                                    (list (make-block 4 5 'red)
                                                          (make-block 5 4 'red)
                                                          (make-block 6 5 'red))) empty 0))
              (make-world (make-tetra (make-posn 5 5)
                                      (list (make-block 5 4 'red)
                                            (make-block 6 5 'red)
                                            (make-block 5 6 'red))) empty 0))

(check-expect (tetra-rotate 'left 
                            (make-world (make-tetra (make-posn 5 5)
                                                    (list (make-block 4 5 'red)
                                                          (make-block 5 4 'red)
                                                          (make-block 6 5 'red))) empty 0))
              (make-world (make-tetra (make-posn 5 5)
                                      (list (make-block 5 6 'red)
                                            (make-block 4 5 'red)
                                            (make-block 5 4 'red))) empty 0))

#;(check-expect (tetra-rotate 'left 
                              (make-world (make-tetra (make-posn 0 1 )
                                                      (list (make-block 0 0 'red)
                                                            (make-block 5 4 'red)
                                                            (make-block 6 5 'red))) empty 0))
                (make-world (make-tetra (make-posn 0 1)
                                        (list (make-block 0 0 'red)
                                              (make-block 5 4 'red)
                                              (make-block 6 5 'red))) empty 0))

#;(check-expect (tetra-rotate 'right 
                              (make-world (make-tetra (make-posn 19 1 )
                                                      (list (make-block 19 0 'red)
                                                            (make-block 5 4 'red)
                                                            (make-block 6 5 'red))) empty 0))
                (make-world (make-tetra (make-posn 19 1)
                                        (list (make-block 19 0 'red)
                                              (make-block 5 4 'red)
                                              (make-block 6 5 'red))) empty 0))

(define (tetra-rotate dir world)
  (cond [(and (symbol? dir) (not (rotate-collision? dir world)))
         (make-world (make-tetra (tetra-center (world-tetra world))
                                 (rotate-bset dir 
                                              (tetra-center (world-tetra world)) 
                                              (tetra-blocks (world-tetra world))))                                
                     (world-pile world) 
                     (world-score world))]
        [else world]))

;;; rotate-bset : Direction Posn BSet -> BSet
;;; Rotates all of the blocks in a BSet
(check-expect (rotate-bset 'left (make-posn 5 5) empty)
              empty)

(check-expect (rotate-bset 'left (make-posn 5 5) (list (make-block 6 5 'red)
                                                       (make-block 5 6 'red)))
              (list (make-block 5 4 'red)
                    (make-block 6 5 'red)))

(check-expect (rotate-bset 'right (make-posn 5 5) (list (make-block 6 5 'red)
                                                        (make-block 5 6 'red ))) 
              (list (make-block 5 6 'red)
                    (make-block 4 5 'red)))

(define (rotate-bset dir posn bset)
  (cond 
    [(empty? bset) empty]
    [(symbol=? 'left dir) 
     (cons (block-rotate-ccw posn (first bset)) (rotate-bset dir posn (rest bset)))]
    [(symbol=? 'right dir) 
     (cons (block-rotate-cw posn (first bset)) (rotate-bset dir posn (rest bset)))]
    [else bset]))

;;; block-rotate : Direction Posn Block -> Block
;;; Rotates the block in the given direction
(check-expect (block-rotate 'left (make-posn 5 5)
                            (make-block 6 5 'red))
              (make-block 5 4 'red))
(check-expect (block-rotate 'right (make-posn 5 5)
                            (make-block 6 5 'red))
              (make-block 5 6 'red))

(define (block-rotate dir posn block)
  (cond [(symbol=? 'left dir) (block-rotate-ccw posn block)]
        [(symbol=? 'right dir) (block-rotate-cw posn block)]))


;;; block-rotate-ccw : Posn Block -> Block
;;; Rotate the block 90 counterclockwise around the posn.
(check-expect (block-rotate-ccw (make-posn 5 5) (make-block 6 5 'red))
              (make-block 5 4 'red))

(check-expect (block-rotate-ccw (make-posn 5 5) (make-block 5 4 'red))
              (make-block 4 5 'red))

(define (block-rotate-ccw c b)
  (block-rotate-cw c (block-rotate-cw c (block-rotate-cw c b))))

;;; block-rotate-cw : Posn Block -> Block
;;; Rotate the block 90 clockwise around the posn.
(check-expect (block-rotate-cw (make-posn 5 5) (make-block 6 5 'red))
              (make-block 5 6 'red))

(check-expect (block-rotate-cw (make-posn 5 5) (make-block 5 6 'red))
              (make-block 4 5 'red))

(define (block-rotate-cw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

;;; rotate-collision? : Direction World -> Boolean
;;; Checks if rotating the tetra in the given direction will cause it to collide
(check-expect (rotate-collision? 'right (make-world
                                         (make-tetra (make-posn 5 5)
                                                     (list
                                                      (make-block 6 4 'blue)
                                                      (make-block 6 5 'blue)))
                                         (list
                                          (make-block 4 5 'blue)
                                          (make-block 5 6 'blue))
                                         0)) true)
(check-expect (rotate-collision? 'right (make-world
                                         (make-tetra (make-posn 5 5)
                                                     (list
                                                      (make-block 6 4 'blue)
                                                      (make-block 6 5 'blue)))
                                         (list
                                          (make-block 7 7 'blue)
                                          (make-block 6 7 'blue))
                                         0)) false)
(check-expect (rotate-collision? 'left (make-world
                                        (make-tetra (make-posn 5 5)
                                                    (list
                                                     (make-block 6 4 'blue)
                                                     (make-block 6 5 'blue)))
                                        (list
                                         (make-block 4 5 'blue)
                                         (make-block 5 4 'blue)) 0)) true)
(check-expect (rotate-collision? 'left (make-world
                                        (make-tetra (make-posn 5 5)
                                                    (list
                                                     (make-block 6 4 'blue)
                                                     (make-block 6 5 'blue)))
                                        (list
                                         (make-block 8 8 'blue)
                                         (make-block 8 8 'blue)) 0)) false)
(check-expect (rotate-collision? 'left (make-world
                                          (make-tetra (make-posn 0 5)
                                                      (list (make-block 0 4 'blue)))
                                          empty 0)) true)
(check-expect (rotate-collision? 'right (make-world
                                           (make-tetra (make-posn 19 5)
                                                       (list (make-block 19 4 'blue)))
                                           empty 0)) true)

(define (rotate-collision? dir w)
  (or (bset-overlap? 
       (rotate-bset dir (tetra-center (world-tetra w)) 
                    (tetra-blocks (world-tetra w))) (world-pile w))
      (rotate-collide-wall? 
       (rotate-bset dir (tetra-center (world-tetra w)) 
                    (tetra-blocks (world-tetra w))))))

;;; bset-overlap? : BSet BSet -> Boolean
;;; Checks if any two parts of the BSets are overlaping
(check-expect (bset-overlap? (list
                              (make-block 4 5 'blue)
                              (make-block 3 5 'blue))
                             (list 
                              (make-block 4 5 'blue)
                              (make-block 5 5 'blue))) true)
(check-expect (bset-overlap? empty (list
                                    (make-block 4 4 'blue))) false)
(check-expect (bset-overlap? (list
                              (make-block 4 5 'blue)
                              (make-block 5 5 'blue))
                             (list 
                              (make-block 4 6 'blue)
                              (make-block 5 6 'blue))) false)
(define (bset-overlap? bset1 bset2)
  (cond
    [(empty? bset1) false]
    [else (or (block-overlap-bset? (first bset1) bset2) (bset-overlap? (rest bset1) bset2))]))

;;; block-overlap-bset? : Block BSet -> Boolean
;;; Checks if the block overlaps with any of the blocks in the bset
(check-expect (block-overlap-bset?
               (make-block 4 5 'blue)
               (list 
                (make-block 4 6 'blue)
                (make-block 5 6 'blue))) false)
(check-expect (block-overlap-bset?
               (make-block 5 6 'blue)
               (list 
                (make-block 4 6 'blue)
                (make-block 5 6 'blue))) true)
(check-expect (block-overlap-bset?
               (make-block 5 6 'blue) empty)
              false)

(define (block-overlap-bset? block bset)
  (cond 
    [(empty? bset) false]
    [else (or (block-overlap? block (first bset))
              (block-overlap-bset? block (rest bset)))]))

;;; block-overlap? : Block Block -> Boolean
;;; Checks if the two blocks overlap
(check-expect (block-overlap? (make-block 4 4 'purple) (make-block 4 4 'purple)) true)
(check-expect (block-overlap? (make-block 4 4 'purple) (make-block 5 4 'purple)) false)

(define (block-overlap? block1 block2)
  (and (= (block-x block1) (block-x block2))
       (= (block-y block1) (block-y block2))))

;;; rotate-collide-wall? : BSet -> Boolean
;;; Checks if the bset will collide with the wall on rotate.
(check-expect (rotate-collide-wall? empty) false)
(check-expect (rotate-collide-wall? (list
                                     (make-block -1 3 'blue)
                                     (make-block 0 2 'blue))) true)
(check-expect (rotate-collide-wall? (list
                                     (make-block 20 3 'blue)
                                     (make-block 19 3 'blue))) true)
(check-expect (rotate-collide-wall? (list
                                     (make-block 4 0 'blue)
                                     (make-block 9 3 'blue))) false)
(check-expect (rotate-collide-wall? (list
                                     (make-block 2 3 'blue)
                                     (make-block 5 3 'blue))) false)
(check-expect (rotate-collide-wall? (list
                                     (make-block 14 -1 'blue)
                                     (make-block 14 0 'blue))) true)
(check-expect (rotate-collide-wall? (list
                                     (make-block 14 20 'blue)
                                     (make-block 14 19 'blue))) true)
(check-expect (rotate-collide-wall? (list
                                     (make-block 9 0 'blue)
                                     (make-block 9 1 'blue))) false)
(define (rotate-collide-wall? bset)
  (cond
    [(empty? bset) false]
    [else (or (block-out? (first bset)) (rotate-collide-wall? (rest bset)))]))

;;; block-out? : Block -> Boolean
;;; Checks if the block is inside the board.
(check-expect (block-out? (make-block 9 4 'purple)) false)
(check-expect (block-out? (make-block 4 20 'purple)) true)
(check-expect (block-out? (make-block 4 -1 'purple)) true)
(check-expect (block-out? (make-block -1 4 'purple)) true)
(check-expect (block-out? (make-block 10 4 'purple)) true)

(define (block-out? block)
  (or (< (block-x block) 0) 
      (>= (block-x block) BOARD-WIDTH)
      (< (block-y block) 0)
      (>= (block-y block) BOARD-HEIGHT)))


;;; move-sideways : Direction World -> World
;;;Shifts the tetra horizontally left or right
(check-expect (move-sideways 'left 
                             (make-world 
                              (make-tetra (make-posn 3 0)
                                          (list (make-block 2 1 'limegreen)
                                                (make-block 3 1 'limegreen)
                                                (make-block 3 0 'limegreen)
                                                (make-block 4 0 'limegreen))) empty 0))
              (make-world 
               (make-tetra (make-posn 2 0)
                           (list (make-block 1 1 'limegreen)
                                 (make-block 2 1 'limegreen)
                                 (make-block 2 0 'limegreen)
                                 (make-block 3 0 'limegreen))) empty 0))     

(check-expect (move-sideways 'left 
                             (make-world (make-tetra (make-posn 3 0)
                                                     (list (make-block 0 0 'limegreen)
                                                           (make-block 3 1 'limegreen)
                                                           (make-block 3 0 'limegreen)
                                                           (make-block 4 0 'limegreen))) empty 0)) 
              (make-world (make-tetra (make-posn 3 0)
                                      (list (make-block 0 0 'limegreen)
                                            (make-block 3 1 'limegreen)
                                            (make-block 3 0 'limegreen)
                                            (make-block 4 0 'limegreen))) empty 0))

(check-expect (move-sideways 'right 
                             (make-world (make-tetra (make-posn 3 0)
                                                     (list (make-block 2 1 'limegreen)
                                                           (make-block 3 1 'limegreen)
                                                           (make-block 3 0 'limegreen)
                                                           (make-block 4 0 'limegreen))) empty 0)) 
              (make-world (make-tetra (make-posn 4 0)
                                      (list (make-block 3 1 'limegreen)
                                            (make-block 4 1 'limegreen)
                                            (make-block 4 0 'limegreen)
                                            (make-block 5 0 'limegreen))) empty 0))

(check-expect (move-sideways 'right 
                             (make-world (make-tetra (make-posn 3 0)
                                                     (list (make-block 19 1 'limegreen)
                                                           (make-block 3 1 'limegreen)
                                                           (make-block 3 0 'limegreen)
                                                           (make-block 4 0 'limegreen))) empty 0)) 
              (make-world (make-tetra (make-posn 3 0)
                                      (list (make-block 19 1 'limegreen)
                                            (make-block 3 1 'limegreen)
                                            (make-block 3 0 'limegreen)
                                            (make-block 4 0 'limegreen))) empty 0))

(define (move-sideways dir world)
  (cond [(and (symbol=? dir 'left) (not (side-collide? dir (tetra-blocks (world-tetra world))
                                                       (world-pile world))))  
         (make-world (add-to-tetra-x (world-tetra world) -1) 
                     (world-pile world) 
                     (world-score world))]
        [(and (symbol=? dir 'right) (not (side-collide? dir (tetra-blocks (world-tetra world))
                                                        (world-pile world)))) 
         (make-world (add-to-tetra-x (world-tetra world) 1) 
                     (world-pile world) 
                     (world-score world))]
        [else world]))
;;; side-collide? : Direction BSet BSet -> Boolean
;;; Checks is anything if a BSet is horizontally adjacent to the world boundary or
;;; another BSet
(check-expect (side-collide? 'left
                             (list (make-block 0 1 'limegreen)
                                   (make-block 1 1 'limegreen)
                                   (make-block 1 0 'limegreen)
                                   (make-block 2 0 'limegreen))
                             empty) true)

(check-expect (side-collide? 'left 
                             (list (make-block 5 5 'red)
                                   (make-block 4 5 'red))
                             (list (make-block 3 5 'blue)))
              true)

(check-expect (side-collide? 'left 
                             (list (make-block 9 5 'red)
                                   (make-block 3 5 'red))
                             empty) false)



(check-expect (side-collide? 'right 
                             (list (make-block 9 5 'red)
                                   (make-block 8 5 'red))
                             empty) true)

(check-expect (side-collide? 'right 
                             (list (make-block 0 5 'red)
                                   (make-block 8 5 'red))
                             empty) false)

(check-expect (side-collide? 'right 
                             (list (make-block 5 5 'red)
                                   (make-block 4 5 'red))
                             (list (make-block 3 5 'blue)))
              false)

(define (side-collide? dir tetra-bset pile-bset)
  (or (side-wall-collide? dir tetra-bset) (side-bset-collide? dir tetra-bset pile-bset)))


;;; side-wall-collide? : Direction BSet -> Boolean
;;; Checks if a BSet is horizontally adjacent to a wall in the given direction.
(check-expect (side-wall-collide? 'left 
                                  
                                  (list (make-block 0 1 'limegreen)
                                        (make-block 1 1 'limegreen)
                                        (make-block 1 0 'limegreen)
                                        (make-block 2 0 'limegreen))) true)

(check-expect (side-wall-collide? 'right 
                                  
                                  (list (make-block 8 1 'limegreen)
                                        (make-block 9 1 'limegreen)
                                        (make-block 9 0 'limegreen)
                                        (make-block 9 0 'limegreen))) true)

(check-expect (side-wall-collide? 'left 
                                  (list (make-block 5 5 'red)
                                        (make-block 4 5 'red))) false)

(check-expect (side-wall-collide? 'left empty) false)

(define (side-wall-collide? dir bset)
  (cond [(empty? bset) false]   
        [(symbol=? 'left dir) (or (<= (block-x (first bset)) 0) 
                                  (side-wall-collide? dir (rest bset)))]    
        [(symbol=? 'right dir) (or (>= (block-x (first bset)) 
                                       (- BOARD-WIDTH 1))
                                   (side-wall-collide? dir (rest bset)))])) 

;;; side-bset-collide? : Direction BSet BSet -> Boolean
;;; Checks if the tetra BSet has collided with the other BSet 
;;; in the given lateral direction.
(check-expect (side-bset-collide? 'left (list (make-block 0 1 'blue)
                                              (make-block 0 2 'blue))
                                  empty) false)
(check-expect (side-bset-collide? 'left 
                                  (list (make-block 1 1 'limegreen)
                                        (make-block 2 1 'limegreen)
                                        (make-block 2 0 'limegreen)
                                        (make-block 3 0 'limegreen))
                                  (list (make-block 0 1 'blue)
                                        (make-block 0 2 'blue))) true)

(check-expect (side-bset-collide? 'right 
                                  (list (make-block 6 1 'blue)
                                        (make-block 7 1 'blue)
                                        (make-block 7 0 'blue)
                                        (make-block 8 0 'blue))
                                  (list (make-block 8 1 'limegreen)
                                        (make-block 9 1 'limegreen)
                                        (make-block 9 0 'limegreen)
                                        (make-block 9 0 'limegreen))) true)

(check-expect (side-bset-collide? 'left 
                                  (list (make-block 2 2 'red)
                                        (make-block 1 2 'red))
                                  (list (make-block 5 5 'red)
                                        (make-block 4 5 'red))) false)
(define (side-bset-collide? dir tetra-bset pile)
  (cond
    [(empty? tetra-bset) false]
    [else (or (block-adjacent-bset-x dir (first tetra-bset) pile)
              (side-bset-collide? dir (rest tetra-bset) pile))]))

;;; block-adjacent-bset-x : Direction Block BSet -> Boolean
;;; Checks if the block is adjacent to any of the blocks in the BSet
(check-expect (block-adjacent-bset-x 'left (make-block 4 4 'blue)
                                     empty) false)
(check-expect (block-adjacent-bset-x 'right (make-block 4 4 'blue)
                                     (list (make-block 6 5 'blue)
                                           (make-block 6 4 'blue)
                                           (make-block 5 4 'blue))) true)
(check-expect (block-adjacent-bset-x 'left (make-block 4 4 'blue)
                                     (list (make-block 6 5 'blue)
                                           (make-block 6 4 'blue)
                                           (make-block 5 4 'blue))) false)
(check-expect (block-adjacent-bset-x 'left (make-block 4 4 'blue)
                                     (list (make-block 3 4 'blue)
                                           (make-block 2 4 'blue)
                                           (make-block 2 3 'blue))) true)
(define (block-adjacent-bset-x dir block bset)
  (cond
    [(empty? bset) false]
    [else (or (block-adjacent-y dir block (first bset))
              (block-adjacent-bset-x dir block (rest bset)))]))

;;; block-adjacent-y : Direction Block Block -> Boolean
;;; Checks if the two blocks are adjacent in the vertical direction given.
;;; eg If left is given it checks to the "left" of the first block.
(check-expect (block-adjacent-y 'left (make-block 4 4 'blue) (make-block 3 4 'blue)) true)
(check-expect (block-adjacent-y 'right (make-block 4 4 'blue) (make-block 5 4 'blue)) true)
(check-expect (block-adjacent-y 'right (make-block 4 4 'blue) (make-block 3 4 'blue)) false)
(check-expect (block-adjacent-y 'left (make-block 4 4 'blue) (make-block 5 4 'blue)) false)

(define (block-adjacent-y dir block1 block2)
  (cond
    [(symbol=? dir 'left) (and (= (- (block-x block1) 1) (block-x block2))
                               (= (block-y block1) (block-y block2)))]
    [(symbol=? dir 'right) (and (= (+ (block-x block1) 1) (block-x block2))
                                (= (block-y block1) (block-y block2)))]))

(define (main initial-world)
  (big-bang initial-world
            (on-tick next-world .5)
            (to-draw draw-board)
            (on-key key-handler)))