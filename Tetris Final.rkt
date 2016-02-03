;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ProblemSet8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;;; Problem #1 ;;;
;;; sort-n : [Number Number -> Boolean] [List-of Number] -> [List-of Number]

;;; sort-n : [String String -> Boolean] [List-of String] -> [List-of String]

;;; sort : [X X -> Boolean] [List-of X] -> [List-of X]

;;; sort-ir : [IR IR -> Boolean] [List-of String] -> [List-of X]

;;; Problem #2 ;;;
(define-struct toy (name desc acq-price sale-price))
;;; A Toy is a (make-toy string string number number)
;;; Interpretation: A toy has a given name, description, acquisition price, 
;;; and recommended sales price

;;; A LOT is a [List-of Toy]

;;; eliminate-exp : Number [List-of Toy] -> [List-of Toy]
;;; Returns a list of toys with a sales price under the inputed ua

(check-expect (eliminate-exp 12 (list (make-toy "bear" "a bear" 20 11)
                                      (make-toy "jack-in-the-box" "box" 10 20)
                                      (make-toy "something-else" "anything" 11 1)))
              (list (make-toy "bear" "a bear" 20 11)
                    (make-toy "something-else" "anything" 11 1)))

(check-expect (eliminate-exp 10 empty)
              empty)

(define (eliminate-exp ua lot)
  (filter (lambda (toy) (< (toy-sale-price toy) ua)) lot)) 

;;; Recall : String [List-of Toy] -> [List-of Toy]
;;; Returns a list of toys that do no use the name ty

(check-expect (recall "Ghost" (list (make-toy "NGHOST" "not-ghost" 1 2)
                                    (make-toy "Ghost" "ghostss" 2 1)
                                    (make-toy "Dog" "Ghost" 3 3)))
              (list (make-toy "NGHOST" "not-ghost" 1 2)
                    (make-toy "Dog" "Ghost" 3 3)))

(check-expect (recall "Ping-Pong" empty)
              empty)

(define (recall name lot)
  (filter (lambda (toy) (not (string=? (toy-name toy) name))) lot))

;;; Selection : [List-of String] [List-of String] -> [List-of String]
;;; Returns a list of the strings from the second list that are also on the first
(check-expect (selection (list "Toy1" "Toy2" "Toy3" "Toy4")
                         (list "Toy2" "Toy3"))
              (list "Toy2" "Toy3"))

(check-expect (selection empty empty)
              empty)

(check-expect (selection empty (list "Toy5"))
              empty)

(check-expect (selection (list "Toy6") empty)
              empty)

(define (selection los1 los2)
  (filter (lambda (item1) (member? item1 los1)) los2))  

;;; Problem 3 ;;; 
;;; We defined the code for checking purposes. We understand lambda expressions
;;; do not need names.

(check-expect (part1 5) true)
(check-expect (part1 11) false)
(define part1 (lambda (num) (< num 10)))

(check-expect (part2 3 2) "6")
(define part2 (lambda (num1 num2) (number->string (* num1 num2))))

(check-expect (part3 (make-posn 2 0) (rectangle 3 4 "solid" 'red))
              (place-image (circle 1.5 "solid" 'red) 2 0 (rectangle 3 4 "solid" 'red)))
(define part3 (lambda (posn rect) (place-image (circle 1.5 "solid" 'red) 
                                               (posn-x posn) 
                                               (posn-y posn) rect)))

(define-struct ir [name price])
; An IR is 
;   (make-ir String Number)
; An loir is an: [List-of IR]
(check-expect (part4 (list (make-ir "two" 2) 
                           (make-ir "one" 1) 
                           (make-ir "four" 4) 
                           (make-ir "three" 3)))
              (list (make-ir "one" 1) (make-ir "two" 2) (make-ir "three" 3) (make-ir "four" 4)))
(check-expect (part4 empty) empty)
(define part4 (lambda (loir) (sort loir (lambda (ir1 ir2)
                                          (< (ir-price ir1) (ir-price ir2))))))

(check-expect (part5 2) 0)
(check-expect (part5 5) 1)
(define part5 (lambda (nat-num)
                (cond [(odd? nat-num) 1]
                      [else 0])))

(require 2htdp/image)
(require 2htdp/universe)

;;; Problem 4 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TETRIS ;;;
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
(define FAST-SPEED 1)  ; in grid cells 

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
                               4))

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
                                         (make-block 6 2 'limegreen)) 4))


;;------------------------------------------------------------------------------
;;; Main Function ;;;

(define (run initial-world)
  (big-bang initial-world
            (on-tick next-world .5)
            (to-draw draw-board)
            (on-key key-handler)))

;;------------------------------------------------------------------------------
;;; Render System ;;;

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
              (place-bset (tetra-blocks S-START) 
                          (place-bset 
                           (list (make-block 3 18 'limegreen) 
                                 (make-block 4 18 'limegreen)
                                 (make-block 5 18 'limegreen)
                                 (make-block 6 18 'limegreen))
                           BOARD))))

(check-expect (draw-board FAIL-WORLD)
              (place-image/grid 
               (text "Press R" 18 'black) 4.5 16
               (place-image/grid 
                (text "Press R" 20 'white) 4.5 16
                (place-image/grid 
                 (text "to Continue" 18 'black) 4.5 18
                 (place-image/grid 
                  (text "to Continue" 20 'white) 4.5 18
                  (place-image/grid 
                   (text "SCORE" 25 'black) 4.5 7
                   (place-image/grid 
                    (text "SCORE" 27 'white) 4.5 7
                    (place-image/grid 
                     (text "4" 45 'black) 4.5 10 
                     (place-image/grid 
                      (text "4" 47 'white) 4.5 10 
                      (place-bset 
                       (list (make-block 3 0 'limegreen) 
                             (make-block 4 18 'limegreen)
                             (make-block 5 18 'limegreen)
                             (make-block 6 18 'limegreen)) 
                       BOARD))))))))))

(define (draw-board world)
  (local (;; place-tetra : Tetra Image -> Image
          ;; Places the Tetra on an image
          (define (place-tetra tetra image)
            (place-bset (tetra-blocks tetra) image)))
    (cond [(lose? (world-pile world)) (display-end world)]
          [else (place-score 
                 (world-score world) 
                 (place-tetra 
                  (world-tetra world) 
                  (place-bset 
                   (world-pile world) BOARD)))]))) 

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
        [else (place-image/grid 
               OUTLINE
               (block-x (first bset))
               (block-y (first bset))  
               (place-image/grid 
                (square 10 "solid" (block-color (first bset))) 
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
               (text "Press R" 18 'black) 4.5 16
               (place-image/grid 
                (text "Press R" 20 'white) 4.5 16
                (place-image/grid 
                 (text "to Continue" 18 'black) 4.5 18
                 (place-image/grid 
                  (text "to Continue" 20 'white) 4.5 18
                  (place-image/grid 
                   (text "SCORE" 25 'black) 4.5 7
                   (place-image/grid 
                    (text "SCORE" 27 'white) 4.5 7
                    (place-image/grid 
                     (text "4" 45 'black) 4.5 10 
                     (place-image/grid 
                      (text "4" 47 'white) 4.5 10 
                      (place-bset 
                       (list (make-block 3 0 'limegreen) 
                             (make-block 4 18 'limegreen)
                             (make-block 5 18 'limegreen)
                             (make-block 6 18 'limegreen)) 
                       BOARD))))))))))

(define (display-end world)
  (place-image/grid 
   (text "Press R" 18 'black) 4.5 16
   (place-image/grid 
    (text "Press R" 20 'white) 4.5 16
    (place-image/grid 
     (text "to Continue" 18 'black) 4.5 18
     (place-image/grid 
      (text "to Continue" 20 'white) 4.5 18
      (place-image/grid 
       (text "SCORE" 25 'black) 4.5 7
       (place-image/grid 
        (text "SCORE" 27 'white) 4.5 7
        (place-image/grid 
         (text (number->string (world-score world))
               45 'black) 
         4.5 10
         (place-image/grid 
          (text (number->string (world-score world))
                47 'white) 4.5 10
                           (place-bset (world-pile world) 
                                       BOARD))))))))))

;;; place-image/grid : Image Number Number Image -> Image
;;; Places an image at the given grid coordinates on a board.
;;; Restrictions: the two grid coordinates (Number Number) must be positive
(check-expect (place-image/grid (square 10 "solid" 'red) 
                                5 5 
                                (empty-scene 100 100))
              (place-image (square 10 "solid" 'red) 
                           55 55 
                           (empty-scene 100 100)))
(check-expect (place-image/grid (square 10 "solid" 'red) 
                                9 2 
                                (empty-scene 100 100))
              (place-image (square 10 "solid" 'red) 
                           95 25 
                           (empty-scene 100 100)))
(check-expect (place-image/grid (square 10 "solid" 'red) 
                                2 7 
                                (empty-scene 100 100))
              (place-image (square 10 "solid" 'red) 
                           25 75 
                           (empty-scene 100 100)))

(define (place-image/grid image x y scene)
  (place-image image (* (+ x .5) PIXELS/GRID) (* (+ y .5) PIXELS/GRID) scene))

;;------------------------------------------------------------------------------
;;; World Update Functions ;;;

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
              (make-world  ((lambda (n)(get-tetra (random 7))) 4)
                           (list
                            (make-block 3 1 'red) 
                            (make-block 4 1 'red)
                            (make-block 4 0 'red)
                            (make-block 5 0 'red)
                            (make-block 3 2 'limegreen) 
                            (make-block 4 2 'limegreen)
                            (make-block 5 2 'limegreen)
                            (make-block 6 2 'limegreen))
                           8))

(define (next-world w)
  (local ((define (update-score a-bset)
            (cond
              [(empty? a-bset) 0]
              [else (+ 1 (update-score (rest a-bset)))])))    
    (cond
      [(lose? (world-pile w)) (drop-tetra w 0 (update-score (world-pile w)))]
      [(collide-y? w) 
       (make-world (get-tetra (random 7)) 
                   (append (tetra-blocks (world-tetra w)) (world-pile w))                                 
                   (+ (length (tetra-blocks (world-tetra w))) 
                      (world-score w)))]
      [else (drop-tetra w DEFAULT-SPEED (update-score (world-pile w)))])))

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

;;; shift-tetra-x : Tetra Number -> Tetra
;;; Adds the given number to the tetra's x positions
(check-expect (shift-tetra-x S-START 1)
              (make-tetra (make-posn 5 0) 
                          (list (make-block 4 1 'red) 
                                (make-block 5 1 'red)
                                (make-block 5 0 'red)
                                (make-block 6 0 'red))))

(check-expect (shift-tetra-x S-START -1)
              (make-tetra (make-posn 3 0) 
                          (list (make-block 2 1 'red) 
                                (make-block 3 1 'red)
                                (make-block 3 0 'red)
                                (make-block 4 0 'red))))

(define (shift-tetra-x tetra add-x)
  (make-tetra (make-posn (+ add-x (posn-x (tetra-center tetra)))
                         (posn-y (tetra-center tetra)))
              (map (lambda (block)
                     (update-block-pos block 
                                       (+ (block-x block) add-x)
                                       (block-y block))) 
                   (tetra-blocks tetra))))

;;; update-block-pos : Block Number Number -> Block
;;; Updates the block's position with the new x and y
(check-expect (update-block-pos (make-block 14 15 'blue) 12 20)
              (make-block 12 20 'blue))

(check-expect (update-block-pos (make-block 10 50 'green) 5 2)
              (make-block 5 2 'green))

(define (update-block-pos block new-x new-y)
  (make-block new-x new-y (block-color block)))

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
;;---------------------------------------------------------------------------------------------------------------

;;; remove-full-row : BSet -> [List-of BSet]
;;; If there is a full line of blocks in the bset, remove it.
;;; Returns a list of BSets, each one a row.
(check-expect (remove-full-rows empty) empty)
(check-expect (remove-full-rows (list
                                 (make-block 0 1 'blue)
                                 (make-block 4 5 'blue)
                                 (make-block 0 9 'blue)
                                 (make-block 1 9 'blue)
                                 (make-block 2 9 'blue)
                                 (make-block 3 9 'blue)
                                 (make-block 4 9 'blue)
                                 (make-block 5 9 'blue)
                                 (make-block 6 9 'blue)
                                 (make-block 7 9 'blue)
                                 (make-block 8 9 'blue)
                                 (make-block 9 9 'blue)
                                 (make-block 0 2 'blue)))
              (list 
               (list (make-block 0 1 'blue))
               (list (make-block 4 5 'blue))
               (list (make-block 0 2 'blue))))
(check-expect (remove-full-rows (list 
                                 (make-block 0 1 'blue)
                                 (make-block 4 5 'blue)
                                 (make-block 0 2 'blue)))
              (list 
               (list (make-block 0 1 'blue))
               (list (make-block 4 5 'blue))
               (list (make-block 0 2 'blue))))
(check-expect (remove-full-rows (list
                                 (make-block 0 1 'blue)
                                 (make-block 0 6 'blue)
                                 (make-block 1 6 'blue)
                                 (make-block 2 6 'blue)
                                 (make-block 3 6 'blue)
                                 (make-block 4 6 'blue)
                                 (make-block 5 6 'blue)
                                 (make-block 6 6 'blue)
                                 (make-block 7 6 'blue)
                                 (make-block 8 6 'blue)
                                 (make-block 9 6 'blue)
                                 (make-block 4 5 'blue)
                                 (make-block 0 9 'blue)
                                 (make-block 1 9 'blue)
                                 (make-block 2 9 'blue)
                                 (make-block 3 9 'blue)
                                 (make-block 4 9 'blue)
                                 (make-block 5 9 'blue)
                                 (make-block 6 9 'blue)
                                 (make-block 7 9 'blue)
                                 (make-block 8 9 'blue)
                                 (make-block 9 9 'blue)
                                 (make-block 0 2 'blue)))
              (list 
               (list (make-block 0 1 'blue))
               (list (make-block 4 5 'blue))
               (list (make-block 0 2 'blue))))

(define (remove-full-rows bset)
  (local (;;; get-all-rows-of-length : Number [List-of BSet] -> [List-of BSet]
          ;;; Selects the BSets of the given length.
          (define (get-all-rows-of-length num lob)
            (filter (lambda (row) (= (length row) num))
                    lob)))
    (delete-row 
     (get-all-rows-of-length BOARD-WIDTH (get-all-rows bset)) 
     (get-all-rows bset))))

;;; delete-rows : [List-of BSet] [List-of BSet] -> [List-of BSet]
;;; Deletes BSet from the [List-of BSet]
(check-expect (delete-row (list (list (make-block 4 4 'blue))) empty) empty)
(check-expect (delete-row empty (list (list (make-block 4 4 'blue))))
              (list (list (make-block 4 4 'blue))))
(check-expect (delete-row (list (list (make-block 2 2 'green))) 
                          (list (list (make-block 2 3 'blue))
                                (list (make-block 3 4 'green))
                                (list (make-block 2 2 'green))))
              (list (list (make-block 2 3 'blue)) (list (make-block 3 4 'green))))
(check-expect (delete-row (list (list (make-block 2 2 'green))
                                (list (make-block 3 4 'green)))
                          (list (list (make-block 2 3 'blue))
                                (list (make-block 3 4 'green))
                                (list (make-block 2 2 'green)))) 
              (list (list (make-block 2 3 'blue))))

(define (delete-row subset set)
  (filter (lambda (bset) (not (member? bset subset))) set))

;;; get-all-rows : BSet -> [List-of BSet]
;;; Gets a list of the BSet of each row
(check-expect (get-all-rows empty) empty)
(check-expect (get-all-rows
               (list (make-block 0 1 'green)
                     (make-block 0 2 'green)
                     (make-block 1 2 'green)
                     (make-block 3 4 'green)))
              (list
               (list (make-block 0 1 'green))
               (list (make-block 0 2 'green)
                     (make-block 1 2 'green))
               (list (make-block 3 4 'green))))

(define (get-all-rows bset)
  (local (;;; get-block-row : Block BSet -> BSet
          ;;; Gets all of the blocks in the same row as the given block
          (define (get-block-row block bset)
            (filter (lambda (b) (= (block-y b) (block-y block)))
                    bset)))
    (cond [(empty? bset) empty]
          [else (cons (get-block-row (first bset) bset)
                      (get-all-rows (filter (lambda (b) 
                                              (not (= (block-y b) 
                                                      (block-y (first bset))))) 
                                            bset)))])))
;-----------------------------------------------------------------------------------------------------------------
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
        [(key=? "down" key) (if (not (collide-y? w)) 
                                (drop-tetra w FAST-SPEED (world-score w)) w)]
        [(key=? "a" key) (tetra-rotate 'left w)]
        [(key=? "s" key) (tetra-rotate 'right w)]
        [else w]))

;;; tetra-rotate : Direction World -> World
;;; Rotates the tetra in a given direction
(check-expect (tetra-rotate 
               'right 
               (make-world 
                (make-tetra (make-posn 5 5)
                            (list (make-block 4 5 'red)
                                  (make-block 5 4 'red)
                                  (make-block 6 5 'red))) empty 0))
              (make-world 
               (make-tetra (make-posn 5 5)
                           (list (make-block 5 4 'red)
                                 (make-block 6 5 'red)
                                 (make-block 5 6 'red))) empty 0))

(check-expect (tetra-rotate 
               'left 
               (make-world 
                (make-tetra (make-posn 5 5)
                            (list (make-block 4 5 'red)
                                  (make-block 5 4 'red)
                                  (make-block 6 5 'red))) empty 0))
              (make-world 
               (make-tetra (make-posn 5 5)
                           (list (make-block 5 6 'red)
                                 (make-block 4 5 'red)
                                 (make-block 5 4 'red))) empty 0))

(check-expect (tetra-rotate 
               'left 
               (make-world 
                (make-tetra (make-posn 0 1 )
                            (list (make-block 0 0 'red)
                                  (make-block 5 4 'red)
                                  (make-block 6 5 'red))) empty 0))
              (make-world 
               (make-tetra (make-posn 0 1)
                           (list (make-block 0 0 'red)
                                 (make-block 5 4 'red)
                                 (make-block 6 5 'red))) empty 0))

(check-expect (tetra-rotate 
               'right 
               (make-world 
                (make-tetra (make-posn 19 1 )
                            (list (make-block 19 0 'red)
                                  (make-block 5 4 'red)
                                  (make-block 6 5 'red))) empty 0))
              (make-world 
               (make-tetra (make-posn 19 1)
                           (list (make-block 19 0 'red)
                                 (make-block 5 4 'red)
                                 (make-block 6 5 'red))) empty 0))

(define (tetra-rotate dir world)
  (cond [(or (not (symbol? dir)) (rotate-collision? dir world)) world]
        [else (make-world (make-tetra 
                           (tetra-center (world-tetra world))
                           (rotate-bset dir 
                                        (tetra-center (world-tetra world))
                                        (tetra-blocks (world-tetra world))))                                
                          (world-pile world) 
                          (world-score world))]))

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
  (local (;; block-rotate-cw : Posn Block -> Block
          ;; Rotate the block 90 clockwise around the posn.
          (define (block-rotate-cw c b)
            (make-block (+ (posn-x c)
                           (- (posn-y c)
                              (block-y b)))
                        (+ (posn-y c)
                           (- (block-x b)
                              (posn-x c)))
                        (block-color b)))
          ;;; block-rotate : Direction Posn Block -> Block
          ;;; Rotates the block in the given direction
          (define (block-rotate dir posn block)
            (cond [(symbol=? 'left dir) (block-rotate-cw 
                                         posn 
                                         (block-rotate-cw 
                                          posn 
                                          (block-rotate-cw 
                                           posn block)))]
                  [(symbol=? 'right dir) (block-rotate-cw posn block)])))          
    (if (or (not (symbol? dir)) (empty? bset)) bset
        (map (lambda (block) (block-rotate dir posn block)) bset))))


;;; move-sideways : Direction World -> World
;;;Shifts the tetra horizontally left or right
(check-expect (move-sideways 
               'left 
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

(check-expect (move-sideways 
               'left 
               (make-world 
                (make-tetra (make-posn 3 0)
                            (list (make-block 0 0 'limegreen)
                                  (make-block 3 1 'limegreen)
                                  (make-block 3 0 'limegreen)
                                  (make-block 4 0 'limegreen))) empty 0)) 
              (make-world 
               (make-tetra (make-posn 3 0)
                           (list (make-block 0 0 'limegreen)
                                 (make-block 3 1 'limegreen)
                                 (make-block 3 0 'limegreen)
                                 (make-block 4 0 'limegreen))) empty 0))

(check-expect (move-sideways 
               'right 
               (make-world 
                (make-tetra (make-posn 3 0)
                            (list (make-block 2 1 'limegreen)
                                  (make-block 3 1 'limegreen)
                                  (make-block 3 0 'limegreen)
                                  (make-block 4 0 'limegreen))) empty 0)) 
              (make-world 
               (make-tetra (make-posn 4 0)
                           (list (make-block 3 1 'limegreen)
                                 (make-block 4 1 'limegreen)
                                 (make-block 4 0 'limegreen)
                                 (make-block 5 0 'limegreen))) empty 0))

(check-expect (move-sideways
               'right 
               (make-world 
                (make-tetra (make-posn 3 0)
                            (list (make-block 19 1 'limegreen)
                                  (make-block 3 1 'limegreen)
                                  (make-block 3 0 'limegreen)
                                  (make-block 4 0 'limegreen))) empty 0)) 
              (make-world 
               (make-tetra (make-posn 3 0)
                           (list (make-block 19 1 'limegreen)
                                 (make-block 3 1 'limegreen)
                                 (make-block 3 0 'limegreen)
                                 (make-block 4 0 'limegreen))) empty 0))

(define (move-sideways dir world)
  (local (;;; shift-tetra : Direction -> Tetra
          ;;; Shifts the tetra horizontally by 1
          (define (shift-tetra dir)
            (cond [(symbol=? dir 'left) 
                   (shift-tetra-x (world-tetra world) -1)]
                  [else (shift-tetra-x (world-tetra world) 1)])))
    (cond [(side-collide? dir (tetra-blocks (world-tetra world)) 
                          (world-pile world)) world] 
          [else (make-world (shift-tetra dir) (world-pile world) 
                            (world-score world))])))

;;------------------------------------------------------------------------------
;;; COLLISION FUNCTIONS ;;;

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
  (local (;; block-adjacent-y? : Block Block -> Boolean
          ;; Checks to see if two blocks are adjacent top to bottom.
          (define (block-adjacent-y? block1 block2)
            (and (= (- (block-y block2) (block-y block1)) 1)
                 (= (block-x block2) (block-x block1))))
          (define (collide-with-floor? bset)
            (ormap (lambda (block) 
                     (= (+ (block-y block) 1) BOARD-HEIGHT)) bset))
          ;;; bset-adjacent-y? : BSet Bset -> Boolean
          ;;; Checks if two BSets are vertically adjacent bottom to top
          (define (bset-adjacent-y? bset1 bset2)
            ;; ormaps both bset1 and bset2 into the block-adjacent-y?
            (ormap (lambda (b-block) (ormap (lambda (a-block) 
                                              (block-adjacent-y? a-block b-block)) 
                                            bset1)) 
                   bset2))) 
    (or (bset-adjacent-y? (tetra-blocks (world-tetra world)) (world-pile world))
        (collide-with-floor? (tetra-blocks (world-tetra world))))))

;;; rotate-collision? : Direction World -> Boolean
;;; Checks if rotating the tetra in the given direction will cause it to collide
(check-expect 
 (rotate-collision? 
  'right (make-world
          (make-tetra (make-posn 5 5)
                      (list
                       (make-block 6 4 'blue)
                       (make-block 6 5 'blue)))
          (list
           (make-block 4 5 'blue)
           (make-block 5 6 'blue))
          0)) true)
(check-expect 
 (rotate-collision? 
  'right (make-world
          (make-tetra (make-posn 5 5)
                      (list
                       (make-block 6 4 'blue)
                       (make-block 6 5 'blue)))
          (list
           (make-block 7 7 'blue)
           (make-block 6 7 'blue))
          0)) false)
(check-expect 
 (rotate-collision? 
  'left (make-world
         (make-tetra (make-posn 5 5)
                     (list
                      (make-block 6 4 'blue)
                      (make-block 6 5 'blue)))
         (list
          (make-block 4 5 'blue)
          (make-block 5 4 'blue)) 0)) true)
(check-expect 
 (rotate-collision? 
  'left (make-world
         (make-tetra (make-posn 5 5)
                     (list
                      (make-block 6 4 'blue)
                      (make-block 6 5 'blue)))
         (list
          (make-block 8 8 'blue)
          (make-block 8 8 'blue)) 0)) false)
(check-expect 
 (rotate-collision? 
  'left (make-world
         (make-tetra (make-posn 0 5)
                     (list (make-block 0 4 'blue)))
         empty 0)) true)
(check-expect 
 (rotate-collision? 
  'right (make-world
          (make-tetra (make-posn 19 5)
                      (list (make-block 19 4 'blue)))
          empty 0)) true)

(define (rotate-collision? dir w)
  (local (;; bset-outside-board? : BSet -> Boolean
          ;; Checks if the bset is inside the board.
          (define (bset-outside-board? bset)
            (ormap (lambda (block) 
                     (or (< (block-x block) 0) 
                         (< (block-y block) 0)
                         (>= (block-x block) BOARD-WIDTH)
                         (>= (block-y block) BOARD-HEIGHT))) 
                   bset))
          ;;; bset-overlap? : BSet BSet -> Boolean
          ;;; Checks if any two parts of the BSets are overlapping
          (define (bset-overlap? bset1 bset2)
            (ormap (lambda (b-block) 
                     (ormap (lambda (a-block) 
                              (and (= (block-x a-block) (block-x b-block))
                                   (= (block-y a-block) (block-y b-block))))
                            bset1)) 
                   bset2))
          ;;; Rotated Tetra BSet
          (define rotated-tetra (rotate-bset dir (tetra-center (world-tetra w))
                                             (tetra-blocks (world-tetra w)))))
    (or (bset-overlap? rotated-tetra (world-pile w))
        (bset-outside-board? rotated-tetra))))

;;; side-collide? : Direction BSet BSet -> Boolean
;;; Checks if a BSet is horizontally adjacent to the world boundary or another BSet          
(check-expect 
 (side-collide? 'left
                (list (make-block 0 1 'limegreen)
                      (make-block 1 1 'limegreen)
                      (make-block 1 0 'limegreen)
                      (make-block 2 0 'limegreen))
                empty) true)

(check-expect 
 (side-collide? 'left 
                (list (make-block 5 5 'red)
                      (make-block 4 5 'red))
                (list (make-block 3 5 'blue)))
 true)

(check-expect 
 (side-collide? 'left 
                (list (make-block 9 5 'red)
                      (make-block 3 5 'red))
                empty) false)



(check-expect 
 (side-collide? 'right 
                (list (make-block 9 5 'red)
                      (make-block 8 5 'red))
                empty) true)

(check-expect 
 (side-collide? 'right 
                (list (make-block 0 5 'red)
                      (make-block 8 5 'red))
                empty) false)

(check-expect 
 (side-collide? 'right 
                (list (make-block 5 5 'red)
                      (make-block 4 5 'red))
                (list (make-block 3 5 'blue)))
 false)
(define (side-collide? dir tetra-bset pile-bset)
  (or (side-wall-collide? dir tetra-bset) 
      (side-bset-collide? dir tetra-bset pile-bset)))

;;; side-wall-collide? : Direction BSet -> Boolean
;;; Checks if a BSet is horizontally adjacent to a wall in the given direction.
(check-expect 
 (side-wall-collide? 'left 
                     (list (make-block 0 1 'limegreen)
                           (make-block 1 1 'limegreen)
                           (make-block 1 0 'limegreen)
                           (make-block 2 0 'limegreen))) true)

(check-expect 
 (side-wall-collide? 'right 
                     (list (make-block 8 1 'limegreen)
                           (make-block 9 1 'limegreen)
                           (make-block 9 0 'limegreen)
                           (make-block 9 0 'limegreen))) true)

(check-expect 
 (side-wall-collide? 'left 
                     (list (make-block 5 5 'red)
                           (make-block 4 5 'red))) false)

(check-expect 
 (side-wall-collide? 'left empty) false)

(define (side-wall-collide? dir bset)
  (ormap (lambda (block) (cond [(symbol=? 'left dir) (<= (block-x block) 0)]
                               [else (>= (block-x block) (- BOARD-WIDTH 1))])) bset)) 

;;; side-bset-collide? : Direction BSet BSet -> Boolean
;;; Checks if the tetra BSet has collided with the other BSet 
;;; in the given lateral direction.
(check-expect 
 (side-bset-collide? 'left (list (make-block 0 1 'blue)
                                 (make-block 0 2 'blue))
                     empty) false)
(check-expect 
 (side-bset-collide? 'left 
                     (list (make-block 1 1 'limegreen)
                           (make-block 2 1 'limegreen)
                           (make-block 2 0 'limegreen)
                           (make-block 3 0 'limegreen))
                     (list (make-block 0 1 'blue)
                           (make-block 0 2 'blue))) true)

(check-expect 
 (side-bset-collide? 'right 
                     (list (make-block 6 1 'blue)
                           (make-block 7 1 'blue)
                           (make-block 7 0 'blue)
                           (make-block 8 0 'blue))
                     (list (make-block 8 1 'limegreen)
                           (make-block 9 1 'limegreen)
                           (make-block 9 0 'limegreen)
                           (make-block 9 0 'limegreen))) true)

(check-expect 
 (side-bset-collide? 'left 
                     (list (make-block 2 2 'red)
                           (make-block 1 2 'red))
                     (list (make-block 5 5 'red)
                           (make-block 4 5 'red))) false)
(define (side-bset-collide? dir tetra-bset pile)
  (local ((define (plus-or-minus dir) (cond [(symbol=? dir 'left) -]
                                            [else +]))
          ;; block-adjacent-x : Direction Block Block -> Boolean
          ;; Checks if the two blocks are adjacent in the horizontal direction given.
          (define (block-adjacent-x dir block1 block2)
            (cond
              [(symbol? dir) (and (= ((plus-or-minus dir) (block-x block1) 1) 
                                     (block-x block2))
                                  (= (block-y block1) 
                                     (block-y block2)))])))
    ;; ormaps both tetra-bset and pile onto block-adjacent-x
    (ormap (lambda (b-block) 
             (ormap (lambda (a-block) (block-adjacent-x dir b-block a-block)) pile)) 
           tetra-bset)))

;;------------------------------------------------------------------------------
;;; General Utility Functions ;;;

;;; block-pos=? : Block Block -> Boolean
;;; Checks if two Blocks' positions are equal
(check-expect (block-pos=? (make-block 4 4 'blue) (make-block 4 4 'red)) true)
(check-expect (block-pos=? (make-block 4 3 'blue) (make-block 4 4 'blue)) false)
(define (block-pos=? b1 b2)
  (and (= (block-x b1) (block-x b2))
       (= (block-y b1) (block-y b2))))

;;; lose? : BSeT -> Boolean
;;; Determines if the BSeT has reached the top of the screen (fail state)
(check-expect (lose? empty) false)
(check-expect (lose? (world-pile FAIL-WORLD)) true)
(check-expect (lose? (world-pile TEST-WORLD)) false)

(define (lose? bset)
  (ormap (lambda (a-block) (= (block-y a-block) 0)) bset))

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
  (map (lambda (block)
         (update-block-pos block 
                           (block-x block)
                           (+ (block-y block) add-y))) 
       a-bset))

;;; drop-tetra : World Number Number
;;; Recreates a world with the tetra moved down Speed many blocks 
(check-expect (drop-tetra (make-world 
                           (make-tetra (make-posn 3 3) 
                                       (list (make-block 5 5 'red)
                                             (make-block 4 4 'green)))
                           (list (make-block 10 10 'blue)
                                 (make-block 10 11 'blue))
                           100) 5 100)
              (make-world (make-tetra (make-posn 3 8) 
                                      (list (make-block 5 10 'red)
                                            (make-block 4 9 'green)))
                          (list (make-block 10 10 'blue)
                                (make-block 10 11 'blue)) 100))

(define (drop-tetra w speed score)
  (make-world ((lambda (speed1) (update-tetra (world-tetra w) speed1)) speed)
              (world-pile w)
              score))
