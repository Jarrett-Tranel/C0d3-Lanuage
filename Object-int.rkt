;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Object-int) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;constructors

(define class
  (lambda (base-class list-of-field-names list-of-field-values list-of-method-names list-of-method-values)
    (letrec ((create-pairs-for-scope (lambda (lon lov)
                             (cond
                               ((null? lon) '())
                               (else (cons (list (car lon)
                                                 (car lov))
                                           (create-pairs-for-scope (cdr lon) (cdr lov)))))))
             (global-env (list (create-pairs-for-scope (cons list-of-field-names list-of-method-names)
                                                       (cons list-of-field-values list-of-method-values)))))
      (list 'class base-class global-env))))


(define empty-object  
    (class '(no-parent) '() '() (list'(getValue lit)) (list
                                             (lambda (var-name)
                                                    'need-logic-here-PARTOFHW)
                                             'darn-tootin)))

;extractors
(define class->super
  (lambda (class)
    (cadr class)))

(define object->method-names
  (lambda (class)
    (caadr(class->field-values class))))

(define object->method-values
  (lambda (class)
     (cadar(cdr(class->field-values class)))))

(define class->field-values
  (lambda (class)
    (car(caddr class))))

;helpers
(define check-for-method
  (lambda (target-method methods-to-check)
    (cond
      ((null? methods-to-check) #f)
      ((eq? (car methods-to-check) target-method) #t)
      (else (check-for-method target-method (cdr methods-to-check))))))

(define has-methods?
  (lambda (class)
    (cond
      ((eq? (cdr (class->field-values class)) '()) #f)
      (else #t))))
                                                     

;functions
(define sendMessage
  (lambda (who what with)
    (cond
      ((has-methods? who)
        ((check-for-method with (object->method-names who))
         '(success)
         (sendMessage (append '(class) (cdr(class->super who))) what with)))
      ((eq? (class->super who) '(no-parent)) '(Function Not Found))
      (else (sendMessage (append '(class) (cdr(class->super who))) what with)))))

       
      
  
(define Person
  (lambda (list-of-field-names list-of-values)
    (class empty-object list-of-field-names list-of-values '() '())))

(define p1 (Person '(fname lname age) '(Mike Litman 21)))
(define p2 (Person '(fname lname age) '(Dave Smith 18)))

empty-object
(append '(class) (cdr(class->super p1)))
(object->method-names empty-object)
(sendMessage p1 '(getValue) '(fname))



     

  
