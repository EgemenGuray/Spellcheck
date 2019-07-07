;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nestlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; 
;; ***************************************************
;; 

;; (nfoldr f g b nlox) gets two functions f and g base case b and
;; Nest-Listof-X first function is applied when the first element
;; in the list is of type X, and the second is applied when the first
;; element is a list.
;; nfoldr: (X Y -> Y) (Y Y -> Y) Y Nested-Listof-X -> Y
;; Example
(check-expect (flatten '(1 (2 3) () ((4)))) '(1 2 3 4))

(define (nfoldr f g b nlox)
  (cond
    [(empty? nlox) b]
    [(empty? (first nlox)) (nfoldr f g b (rest nlox))]
    [(cons? (first nlox)) (g (nfoldr f g b (first nlox))
                             (nfoldr f g b (rest nlox)))]
    [else (f (first nlox) (nfoldr f g b (rest nlox)))]
    )
  )
;; Test
(check-expect (count-items '(1 (2 3) () ((4)))) 4)

(define (count-items nln) (nfoldr (lambda (x y) (add1 y)) + 0 nln))
(define (flatten lst) (nfoldr cons append empty lst))

;; (nmap fnc nlox) gets a function (fnc) and Nest-List-X
;; applies the given function to each elemnent in the list and
;; returns it
;; nmap: (X -> Y) Nested-Listof-X -> Nested-Listof-Y
;; Example
(check-expect (nmap add1 (list 1 (list 2 3) (list) (list (list 4))))
              (list 2 (list 3 4) (list) (list (list 5))))

(define (nmap fnc nlox)
  (cond
    [(empty? nlox) empty]
    [(empty? (first nlox)) (cons empty (nmap fnc (rest nlox)))]
    [(cons? (first nlox)) (cons (nmap fnc (first nlox))
                                (nmap fnc (rest nlox))) ]
    [else (cons (fnc (first nlox)) (nmap fnc (rest nlox)))]
    )
  )
;; Test
(check-expect (nmap sqr (list 1 (list 2 3) (list) (list (list 4))))
              (list 1 (list 4 9) (list) (list (list 16))))

;; (nfilter fnc nlox) gets a predicate function (fnc) and Nest-List-X
;; filters the elemnents in the list and returns it
;; nfilter: (X -> Bool) Nested-Listof-X -> Nested-Listof-X
;; Example
(check-expect (nfilter even? '(1 (2 3) () ((4)))) '( (2) () ((4))))
(define (nfilter fnc nlox)
  (cond
    [(empty? nlox) empty]
    [(empty? (first nlox)) (cons empty (nfilter fnc (rest nlox)))]
    [(cons? (first nlox)) (cons (nfilter fnc (first nlox))
                                (nfilter fnc (rest nlox))) ]
    [(fnc (first nlox))
     (cons (first nlox) (nfilter fnc (rest nlox)))]
    [else (nfilter fnc (rest nlox))]
    )
  )
;; Test
(check-expect (nfilter odd? '(1 (2 3) () ((4)))) '(1 (3) () (())))

;; (nreverse nlox) gets a Nest-List-X and reverses it
;; nreverse: Nested-Listof-X -> Nested-Listof-X
;; Example
(check-expect (nreverse '(1 (2 3) () ((4)))) '(((4)) () (3 2) 1))

(define (nreverse nlox)
  (cond
    [(empty? nlox) empty]
    [(cons? (first nlox))
     (append (nreverse (rest nlox)) (list (nreverse (first nlox))))]
    [else (append (nreverse (rest nlox)) (list (first nlox)))]
    )
  )
;; Test
(check-expect (nreverse '((1 (2 3)) 4 (5 (6 7 8) 9)))
              '((9 (8 7 6) 5) 4 ((3 2) 1)))


;; (nheight nlox) gets a Nest-List-X and returns its height
;; nheight: Nested-Listof-X -> Nat
;; Example
(check-expect (nheight '((1 (2 3)) 4 (5 (6 7 8) 9))) 3)

(define (nheight nlox)
  (cond
    [(empty? nlox) 1]
    [(cons? (first nlox))
     (max (+ 1 (nheight (first nlox))) (nheight (rest nlox)))]
    [else (max 1 (nheight (rest nlox)))]
    )
  )
;; Test
(check-expect (nheight '()) 1)
(check-expect (nheight '(a b c)) 1)
(check-expect (nheight '((1 a) (2 b) (3 c))) 2)
(check-expect (nheight '(1 (2 3) () ((4)))) 3)

;; (prune nlox) gets a Nest-List-X takes outs the empty lists
;; prune: Nest-List-X -> Nest-List-X
;; Example
(check-expect (prune '(1 (2 3 ()) ( (()) (4) ()(()))))
              (list 1 (list 2 3) (list (list 4))))

(define (prune nlox)
  (local
    [(define lst
       (cond
         [(empty? nlox) empty]
         [(or (empty? (first nlox))
              (equal? '(()) (first nlox))) (prune (rest nlox))]
         [(cons? (first nlox)) (cons (prune (first nlox))
                                     (prune (rest nlox)))]
         [else (cons (first nlox) (prune (rest nlox)))]
    
         ))]
    (cond
      [(empty? lst) empty]
      [(equal? (list(list)) lst) empty]
      [else lst]
      )
    )
  )
;; Test
(check-expect (prune '(()((())()))) empty)
(check-expect (prune '(()((((())))())((())))) empty)


  