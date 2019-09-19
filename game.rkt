#lang slideshow

;*****************************************
;* CONSTANT
;*****************************************
(define GRID_WIDTH 10)
(define GRID_HEIGHT 10)
(define SQUARE_COLOR_FILLED "Green")
(define SQUARE_COLOR_EMPTY "Black")
(define SQUARE_SIZE 12)

;*****************************************
;* GAME RULES
;*****************************************
(define (alive? x) (>= x 1))

;*****************************************
;* DISPLAY GRID
;*****************************************
(define (filled-square n)
  (filled-rectangle n n #:color SQUARE_COLOR_FILLED))

(define (blank-square n)
  (filled-rectangle n n #:color SQUARE_COLOR_EMPTY))

(define (make-square x)
  (if (alive? x) (filled-square SQUARE_SIZE) (blank-square SQUARE_SIZE)))

(define (print-grid m)
  (define (print-row r)
    (for-each print (map make-square r)))
  
    (for-each (lambda (x) (begin
                            (print-row x)
                            (newline)))
              m))

;*****************************************
;* HELPERS
;*****************************************
;(define (random ))
(define (repeat fn times initial)
  (define (repeat-helper fn n result)
    (cond [(> n 0) (repeat-helper fn (- n 1) (fn (- times n) result))]
          [else result]))
  (repeat-helper fn times initial))

(define (make-grid width height fn)
  (define (make-cols line length)
    (repeat (lambda (i lst) (cons (fn i line) lst)) length empty))
  (define (make-rows fn length)
    (repeat (lambda (i lst) (cons (fn i) lst)) length empty))
  (make-rows (lambda (i) (make-cols i width)) height))

(define (make-random-grid width height)
  (make-grid width height (lambda (x y) (if(>= (random 10) 7) 1 0))))

;*****************************************
;* MAIN
;*****************************************

(define (run-game) (begin
     (print-grid (make-random-grid GRID_WIDTH GRID_HEIGHT))              
))
