#lang racket
; Drill: "write a function that squares a number"
; square-number: number -> number

(define (square-number int)
  (* int int))

; Drill "Write a function that converts fahrenheit to celsius"
; fahrenheit-to-celsius number -> number
(define (fahrenheit-to-celsius fahr)
  (/ (* (- fahr 32) 5) 9 ))

; Drill: "Write a function that returns "brrr" if the temperature in celsius is less than zero, or "could be worse" if it is above zero.
; how-cold-is-it number -> str
(define (how-cold-is-it fahr)
  (define celsius (fahrenheit-to-celsius fahr))
  (if (> celsius 0)
      "could be worse"
      "brrr"))

  
; Drill: "Write a function that calculates the surface area of a pipe (area of donuts + area of inner and outer sides).
; pipesurface-area number number number -> number
(define (circle-area radius)
  (* (square-number radius) pi))

(define (circumference radius)
  (* (* radius 2) pi))

(define (pipe-surface-area outer-radius inner-radius length)
  (+ (+ (* (- (circle-area outer-radius) (circle-area inner-radius)) 2) (* length (circumference outer-radius))) (* length (circumference inner-radius))))
  


; Drill: "Write a function that, given a name and a language ("spanish" or "english"), returns a greeting.
; greet str str -> str
(define (greet name language)
  (cond
    [equal? language "spanish" (string-append "Hola " name)]
    [equal? language "english" (string-append "Hello " name)]))


; Drill: Write a function that takes a list and returns the length of that list
;my-length list -> number

(define (my-length list)
  (if (empty? list)
      0
      (+ 1 (my-length (rest list)))))

; Drill: Write a function searches a list for an item and returns the index of that item in the list, or -1 if the item can't be found
;my-search list str -> number

(define (my-search lst item)
  (cond
    ((empty? lst) #f)
    ((equal? (first lst) item) 0)
    (else (let ([result (my-search (rest lst) item)])
                (if result (+ 1 result) #f)))))

(define (tail-search lst item)
  (define (iterate lst index)
    (cond
      ([empty? lst] -1)
      ([equal? (first lst) item] index)
      (else (iterate (rest lst) (+ 1 index)))))
  (iterate lst 0))

; Drill: Write a function that takes a list and an index and returns the item at that index of the list
; my-map list number -> str

(define (my-map lst index)
  (define (iterate lst index)
    (cond
      ([equal? 0 index] (first lst))
      (else (iterate (rest lst) (- index 1)))))
  (if (>= index (my-length lst)) null (iterate lst index)))
      
;Drill: Write a function that takes a list, and returns a new list containing just the first and last element
; first-last-element list -> list

(define (first-last-element lst)
  (list (first lst) (last lst)))

;Drill: Write a function that only returns even numbers from a list
; even-nums-only list -> list
(define (even-nums-only lst)
  (define (iterate iter-lst even-list)
    (cond
      ([empty? iter-lst] even-list)
      ([even? (first iter-lst)]
       (iterate (rest iter-lst) (append even-list (list(first iter-lst)))))
      (else (iterate (rest iter-lst) even-list))))
  (iterate lst '()))

; Write a function that returns items from a list that match a provided criteria.
; select-items fn list -> list

(define (select-items fn lst)
  (define (iterate iter-list selected-items)
    (cond
      ([empty? iter-list] selected-items)
      (else (if (fn (first iter-list))
        (iterate (rest iter-list)
                   (append selected-items (list (first iter-list))))
        (iterate (rest iter-list) selected-items)))))
  (if (not (empty? lst))
    (iterate lst '())
    '()))

(define (my-even? num)
  (equal? 0 (remainder num 2)))

; Write a function that triples all numbers in a list, filters out the odds, and sums the remaining numbers. Use Racket's higher order functions for this.
; triple-even-sum list -> number

(define (triple-even-sum lst)
  (if (empty? lst) 0
      (apply  + (map (lambda (number) (* 3 number)) (select-items my-even? lst)))))
