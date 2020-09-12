#lang racket

; Function menor: returns the smallest integer in a list
(define (menor l)
  (if (= (length l) 1)
      (car l)
      (if (< (car l) (menor (cdr l)))
          (car l)
          (menor (cdr l)))
      )
  )

; Function palindromo: returns the palindrome from 0...N
(define (palindromo-aux-two n)
  (if (= n 0)
      (cons 0 '())
      (cons n (palindromo-aux-two (- n 1)))))

(define (palindromo-aux n c)
  (if (= n c)
      (cons n (palindromo-aux-two n))
      (cons c (palindromo-aux n (+ c 1)))))
                   
(define (palindromo n)
  (palindromo-aux n 0)
  )

; Function negativos: returns a list of only negative numbers of a given list
(define (negativos l)
  (if (empty? l)
      '()
      (if (negative? (car l))
          (append (cons (car l) '()) (negativos (cdr l)))
          (negativos (cdr l))
          )
      )
  )

; Function profundidad: returns the depth of a possible nested list

(define (profundidad l)
  (cond ((null? l) 0)
        ((list? (car l))
         (+ 1 (profundidad (car l))))
        (else 0 + (profundidad (cdr l))))
        
  )

; Function simetrico: returns a balanced sequence of N "(<()>) wheren N=0: () and N=1 (< () >) and so on...
(define (simetrico n)
  (if (= n 0)
      '()
      (list '< (simetrico (- n 1)) '>)))

; Function elimina: removes all appereances of element "dato" in a possible nested list
(define (elimina dato lista)
  (cond [(null? lista) '()]
        [(list? (car lista)) (cons (elimina dato (car lista)) (elimina dato (cdr lista)))]
        [(equal? dato (car lista)) [if (null? (cdr lista))
                                   '()
                                   (elimina dato (cdr lista))
                                   ]]
        [else (cons (car lista) (elimina dato (cdr lista)))]
  )
)





