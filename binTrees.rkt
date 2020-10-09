;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname binTrees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define leaf
  (lambda ()
    '()))

(define at-leaf?
  null?)

(define num->binTree
  (lambda (num)
    (move num (leaf) (leaf))))

(define insertLSON
  (lambda (num binTree)
    (list (currentEl binTree) (move num (leaf) (leaf)) (getRSon binTree))))

(define insertRSON
  (lambda (num binTree)
    (list (currentEl binTree) (getLSon binTree) (move num (leaf) (leaf))))) 

(define move
  (lambda (num lson rson)
    (list num lson rson)))

(define currentEl
  (lambda (binTree)
    (car binTree)))

(define getLSon
    cadr)

(define getRSon
    caddr)

(define tree (num->binTree 13))
(define tree1(insertLSON 7 tree))
(insertRSON 9 tree1)