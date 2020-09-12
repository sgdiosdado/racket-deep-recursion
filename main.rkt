#lang racket

; menor: returns the smallest integer in a list
(define (menor l)
  (if (= (length l) 1)
      (car l)
      (if (< (car l) (menor (cdr l)))
          (car l)
          (menor (cdr l)))
      )
  )

; palindromo: returns the palindrome from 0...N
(define (appendAndShift l n)
  (append (reverse (append l (cons n '()))) (cons n '()))
  )

(define (palindromo n)
  (if (= n 0)
      (appendAndShift '() 0)
      (appendAndShift (palindromo (- n 1)) n)
      )
  )

; negativos: returns a list of only negative numbers of a given list
(define (negativos l)
  (if (empty? l)
      '()
      (if (negative? (car l))
          (append (cons (car l) '()) (negativos (cdr l)))
          (negativos (cdr l))
          )
      )
  )