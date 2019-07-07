;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; ***************************************************
;; 


;; (keep-ints lst) function generates a list which only consist of the
;; number elements of the given list
;; keep-ints: (listof Any) -> (listof Num)
(define (keep-ints lst)
  (filter number? lst)
  )

;; Tests
(check-expect (keep-ints
               (cons 'a (cons 1 (cons 2 (cons 'b (cons 3 empty))))))
              (cons 1 (cons 2 (cons 3 empty))))

;; (contains? elem lst) functions gets an element(elem) and list(list)
;; to determine whether the given list contains the given element
;; or not if contains function returns true else returns false
;; contains?: Any (listof Any) -> Bool
;; Example
(check-expect
 (contains? 'fun (cons 'racket (cons 'is (cons 'fun empty)))) true)

(define (contains? elem lst)
  (not
   (equal?
    (length lst)
    (length (filter false? (map (lambda (x) (equal? elem x)) lst)))))
  )
;; Tests
(check-expect (contains? 'fun
                         (cons false (cons 9 (cons 'fun empty))))true)
(check-expect (contains? 9
                         (cons false (cons 9 (cons 'fun empty))))true)
(check-expect (contains? "a" (list 'a 'b 'c)) false)
(check-expect (contains? "a" (list "a" 'b 'c))true)

;; (lookup-al k alst) gets a key (k) and a listof list(alst)
;; returns the corresponding value of the given key if not present
;; returns empty
;; lookup-al: Nat (listof (listof Nat Any)) -> Any
;; Example
(check-expect (lookup-al 3 (list (list 3 'yarrak)
                                 (list 4 'dummy)
                                 (list 5 'quick)))
              'yarrak)

(define (lookup-al k alst)
  (cond
    [(empty? (filter (lambda (x) (equal? k (first x))) alst))
     empty]
    [else
     (second (first (filter (lambda (x) (equal? k (first x))) alst)))]
    )
  )
;; Test
(check-expect (lookup-al 2 (list (list 3 'yarrak)
                                 (list 4 'dummy)
                                 (list 5 'quick)))
              empty)

;; (extract-keys alst) gets a association list
;; produces a list of keys in the given ALST
;; extract-keys: ALST -> (listof Nat)
;; Example
(check-expect (extract-keys(list (list 3 'yarrak)
                                 (list 4 'dummy)
                                 (list 5 'quick)))
              (list 3 4 5))

(define (extract-keys alst)
  (map (lambda (x) (first x)) alst)
  )
;; Test
(check-expect (extract-keys(list (list 3 'yarrak)
                                 ))
              (list 3))


;; (sum-positive lst) function gets a list consist of only
;; numbers(lst) and returns the addition of only the
;; positive elements of the given list
;; sum-positive: (listof Num) -> Num
;; Example
(check-expect (sum-positive (cons 5(cons -3(cons 4 empty)))) 9)

(define (sum-positive lst)
  (foldr + 0 (filter positive? lst))
  )

;; Tests
(check-expect (sum-positive (cons 5(cons -3(cons 4(cons 0 empty)))))9)
(check-expect (sum-positive (cons 0(cons 0(cons 0 empty)))) 0)
(check-expect (sum-positive (cons -0(cons -0(cons -0 empty)))) 0)
(check-expect (sum-positive empty) 0)

;; (countup-to n b) produces a list from n  to b
;; countup-to: Int Int -> (listof Int)
;; requires n<=b
;; Example
(check-expect (countup-to 6 8) (list 6 7 8))

(define (countup-to n b)
  (build-list (add1 (- b n)) (lambda (x) (+ x n)))
  )
;; Test
(check-expect (countup-to 0 1) (list 0 1))

;; (shout lst) gets a list of strings returns the same list of strings
;; all in uppercase
;; shout: (listof String) -> (listof String)
;; Example
(check-expect (shout '("ege" "men"))(list "EGE" "MEN"))

(define (shout lst)
  (map
   (lambda (x) (list->string (map char-upcase (string->list x)))) lst)
  )
;; Test
(check-expect (shout '("EGE")) '("EGE"))


;; (make-validator loa) gets a list of Any and produces a predicate
;; function which comsumes a single item of type Any and prodcues
;; Boolean
;; make-validator (listof Any) -> (Any -> Bool)
;; Example
(check-expect (primary-colour? 'red) true)

(define (make-validator loa)
  (lambda (x) (equal? (first loa) x))
  )
;; Test
(check-expect (primary-colour? 'blue) false)

(define primary-colour? (make-validator '(red blue green)))
