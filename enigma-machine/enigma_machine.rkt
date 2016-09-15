#lang racket
(define alphabet #hash(("A" . 0) ("B" . 1) ("C" . 2) ("D" . 3) ("E" . 4)
                       ("F" . 5) ("G" . 6) ("H" . 7) ("I" . 8) ("J". 9)
                       ("K" . 10) ("L" . 11) ("M" . 12) ("N" . 13) ("O" . 14)
                       ("P" . 15) ("Q" . 16) ("R" . 17) ("S" . 18) ("T" . 19)
                       ("U" . 20) ("V" . 21) ("W" . 22) ("X" . 23) ("Y" . 24)
                       ("Z" . 25)))
; (hash-ref alphabet "A") -> 0
(define roterI (list "E" "K" "M" "F" "L" "G" "D" "Q" "V" "Z" "N" "T" "O" "W" "Y" "H" "X" "U" "S" "P" "A" "I" "B" "R" "C" "J"))
(define roterII (list "A" "J" "D" "K" "S" "I" "R" "U" "X" "B" "L" "H" "W" "T" "M" "C" "Q" "G" "Z" "N" "P" "Y" "F" "V" "O" "E"))
(define roterIII (list "B" "D" "F" "H" "J" "L" "C" "P" "R" "T" "X" "V" "Z" "N" "Y" "E" "I" "W" "G" "A" "K" "M" "U" "S" "Q" "O"))
(define reflectorA (list "E" "J" "M" "Z" "A" "L" "Y" "X" "V" "B" "W" "F" "C" "R" "Q" "U" "O" "N" "T" "S" "P" "I" "K" "H" "G" "D"))
(define reflectorB (list "Y" "R" "U" "H" "Q" "S" "L" "D" "P" "X" "N" "G" "O" "K" "M" "I" "E" "B" "F" "Z" "C" "W" "V" "J" "A" "T"))
(define reflectorC (list "F" "V" "P" "J" "I" "A" "O" "Y" "E" "D" "R" "Z" "X" "W" "G" "C" "T" "K" "U" "Q" "S" "B" "N" "M" "H" "L"))

(define (tail-search lst item)
  (define (iterate lst index)
    (cond
      ([empty? lst] -1)
      ([equal? (first lst) item] index)
      (else (iterate (rest lst) (+ 1 index)))))
  (iterate lst 0))  

(define (reorder-roter roter letter)
  (let ([index (tail-search roter letter)])
    (cond
      ((> index (hash-ref alphabet letter))
       (append (take-right roter (+ (- 26 index) (hash-ref alphabet letter)))
               (take roter (+ (- 26 index) (hash-ref alphabet letter)))))
      ((< index (hash-ref alphabet letter))
       (append (take-right roter (- (hash-ref alphabet letter) index))
               (take roter (- 26 (- (hash-ref alphabet letter) index)))))
      (else roter))))

(define (stepping-roter roter)
  (append (rest roter) (take roter 1)))

; release 1 - single roter
(define (single-roter roter input)
  (let ([index (hash-ref alphabet input)])
    (list-ref roter index)))

;release 2 - single rotating roter
(define (single-rotating-roter roter ground input)
  (let ([index (hash-ref alphabet input)])
    (list-ref (reorder-roter roter ground) index)))

;release 3 - chaining multiple rotating roters
(define (multiple-rotating-roter roterOne roterTwo roterThree groundOne groundTwo groundThree input)
  (single-rotating-roter roterOne groundOne
                     (single-rotating-roter roterTwo groundTwo
                     (single-rotating-roter roterThree groundThree input))))

;release 4 - multiple rotating roters + reflector
(define (multiple-rotating-roters-with-reflector roterOne roterTwo roterThree groundOne groundTwo groundThree reflector input)
  (multiple-rotating-roter roterThree roterTwo roterOne groundThree groundTwo groundOne
        (single-roter reflector
        (multiple-rotating-roter roterOne roterTwo roterThree groundOne groundTwo groundThree input))))

;release 5 - stepping
(define (string-to-list string lst)
  (if (string=? string "") lst
      (string-to-list (substring string 1 (string-length string)) (append lst (list (substring string 0 1))))))

(define (roter-notch-check roter)
  (cond ([and (equal? (first roter) "U") (equal? (last roter) "X")] true)
        ([and (equal? (first roter) "U") (equal? (last roter) "M")] true)
        ([and (equal? (first roter) "I") (equal? (last roter) "S")] true)
        (else false)))

(define (multiple-roters-with-reflector roterOne roterTwo roterThree reflector input)
  (list (single-roter roterOne (single-roter roterTwo (single-roter roterThree (single-roter reflector (single-roter roterThree (single-roter roterTwo (single-roter roterThree input)))))))))

(define (stepping-roters-with-reflector roterOne roterTwo roterThree groundOne groundTwo groundThree reflector input)
  (define (tail-recursion roterOne roterTwo roterThree reflector input output)
    (set! roterThree (stepping-roter roterThree))
    (cond ([roter-notch-check roterOne]
        (set! roterTwo (stepping-roter roterTwo))))
    (cond ([roter-notch-check roterTwo]
        (set! roterThree (stepping-roter roterThree))
        (set! roterTwo (stepping-roter roterTwo))))
    (if (empty? input) output
        (tail-recursion roterOne roterTwo roterThree reflector (rest input)
                      (append output (multiple-roters-with-reflector roterOne roterTwo roterThree reflector (first input))))))
  (tail-recursion (reorder-roter roterOne groundOne)
                  (reorder-roter roterTwo groundTwo)
                  (reorder-roter roterThree groundThree)
                  reflector
                  (string-to-list input '())
                  '()))