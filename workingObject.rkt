;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname workingObject) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;global-env is a list of scopes where a scope is a list of name/value pairs (2-lists)
;constructors
(define class
  (lambda (base-class list-of-field-names list-of-field-values list-of-method-names list-of-method-values)
    (letrec ((create-pairs-for-scope (lambda (lon lov)
                             (cond
                               ((null? lon) '())
                               (else (cons (list (car lon)
                                                 (car lov))
                                           (create-pairs-for-scope (cdr lon) (cdr lov)))))))
             (global-env (list (create-pairs-for-scope (append list-of-field-names list-of-method-names)
                                                       (append list-of-field-values list-of-method-values)))))
      (list 'class base-class global-env))))
(define empty-object
  (lambda ()
    (class '(no-parent) '() '() '(getValue) (list (lambda(var-name) (list var-name 'it-works))))))
;getters
;env will be a list of classes
;start more local and then bubble out
;sendMessage is going to take in a object,
;then we will need to pass it the whole person
;first we call obj->child
;we look
;if it is not there we look at the parent
(define sendMessage
  (lambda(object param)
    (cond
      ((not(eq?(searchMethod(objLookDeeperChild(obj->child object)) param) #f))(searchMethod(objLookDeeperChild(obj->child object))param))
      (else(searchMethod(objLookDeeperChild(obj->parent object)param))))))
(define searchMethod
  (lambda(messedUpObject param)
    (cond
      ((null? messedUpObject)#f)
      ((eq? (car (car messedUpObject)) param)(car(cdr(car messedUpObject))))
      (else (searchMethod (cdr messedUpObject) param)))))
;pass this in the obj->child method
(define obj->parent
  (lambda(object)
    (car(cdr object))))
(define obj->child
  (lambda(object)
    (car(cdr(cdr object)))))
(define objLookDeeperParent
  (lambda(object)
    (caadr(cdr object))))
(define objLookDeeperChild
  (lambda(personClass)
    (car personClass)))
(define Person
  (lambda (list-of-field-names list-of-values)
    (class (empty-object) list-of-field-names list-of-values '() '())))
(define p1 (Person '(fname lname age) '(Mike Litman 21)))
(define p2 (Person '(fname lname age) '(Dave Smith 18)))
p2
;(sendMessageHelper (objLookDeeper(obj->parent p2)))
(obj->parent p2)
;(car( car ( car (cdr (cdr (car (cdr p2)))))))
(cadr(objLookDeeperChild(obj->child p2)))
;(cadr(car(obj->child p2)))
;(car(car(objLookDeeperChild(obj->child p2))))
;(searchMethod(objLookDeeperChild(obj->child p2)) 'lname)
(sendMessage p2 'age)
;'child
;(obj->parent p2)
;extractors
(define class->super
  (lambda (class)
    (cadr class)))
(define class->list-of-fields
  (lambda (class)
    (caddr class)))