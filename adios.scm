;;; Copyright 2017 Metaphorm Solutions, Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(library (adios)
  (export define-class
          define-class-method
          define-class-field
          define-method
          define-field
          class
          new
          send
          class?
          object?)
  (import (scheme))

  ; Forms
  ; ----------------------------------------------------------------------------
  ; (define class-name (class superclass-name constructor))
  ; (define-class class-name superclass-name constructor)
  ; (super parameter ...)
  ; (define-class-method class-name method-name method-body)
  ; (define-class-method class-name (method-name parameter ...) expr ...)
  ; (define-method class-name method-name method-body)
  ; (define-method class-name (method-name parameter ...) expr ...)
  ; (define object-name (new class-name parameter ...))
  ; (send class-name method-name parameter ...)
  ; (send object-name method-name parameter ...)
  ; (define-class-field class-name field-name field-value)
  ; (send class-name field-name)
  ; (define-field class-name field-name field-value)
  ; (define-field object-name field-name field-value)
  ; (send object-name field-name)
  ; (send this method-name parameter ...)
  ; (class? class-name)
  ; (object? object-name)


  ; Code
  ; ----------------------------------------------------------------------------

  (define-syntax (define-class stx)
    (syntax-case stx ()
      ((define-class class-name superclass-name constructor)
       (with-syntax ((this (datum->syntax #'define-class 'this))
                     (super (datum->syntax #'define-class 'super)))
         #'(define class-name (_class superclass-name
                                      (lambda (this super) constructor)))))
      ((define-class class-name rest ...)
       #'(define class-name (class rest ...)))))

  (define-syntax define-class-method
    (syntax-rules ()
      ((_ class-name (method-name parameter ...) expr ...)
       (class-name 'set-class-attribute 'method-name
                   (lambda (parameter ...) expr ...)))
      ((_ class-name method-name method-body)
       (class-name 'set-class-attribute 'method-name method-body))))

  (define-syntax define-class-field
    (syntax-rules ()
      ((_ class-name field-name field-value)
       (class-name 'set-class-attribute 'field-name field-value))))

  (define-syntax (define-method stx)
    (syntax-case stx ()
      ((define-method parent-name (method-name parameter ...) expr ...)
       (with-syntax ((this (datum->syntax #'define-method 'this))
                     (super (datum->syntax #'define-method 'super)))
         #'(parent-name (set-action parent-name) 'method-name
                        (lambda (this super) (lambda (parameter ...) expr ...)))))
      ((define-method parent-name method-name method-body)
       (with-syntax ((this (datum->syntax #'define-method 'this))
                     (super (datum->syntax #'define-method 'super)))
         #'(parent-name (set-action parent-name)'method-name
                        (lambda (this super) method-body))))))

  (define-syntax define-field
    (syntax-rules ()
      ((_ parent-name field-name field-value)
       (parent-name (set-action parent-name) 'field-name field-value))))

  (define-syntax (class stx)
    (syntax-case stx ()
      ((class superclass constructor)
       (with-syntax ((this (datum->syntax #'class 'this))
                     (super (datum->syntax #'class 'super)))
         #'(_class superclass (lambda (this super) constructor))))
      ((class rest ...)
       #'(_class rest ...))))

  (define (_class . spec)
    (let ((superclass (if (> (length spec) 0) (car spec) (void)))
          (constructor (if (> (length spec) 1) (cadr spec) (void)))
          (class-attributes (make-hash-table))
          (prototype-attributes (make-hash-table)))
      (define this-class
        (lambda (action . parameters)
          (case action
            ('get-class-attribute
             (let ((attribute (hashtable-ref class-attributes (car parameters) (void)))
                   (arguments (cdr parameters)))
               (if (procedure? attribute)
                   (apply attribute arguments)
                   attribute)))
            ('set-class-attribute
             (hashtable-set! class-attributes (car parameters) (cadr parameters)))
            ('get-prototype-attribute
             (let* ((current-object (car parameters))
                    (attribute-name (cadr parameters))
                    (attribute (hashtable-ref prototype-attributes attribute-name (void)))
                    (arguments (cddr parameters)))
               (if (eq? attribute (void))
                   (if (or (eq? superclass (void)) (null? superclass))
                       (void)
                       (apply superclass (cons 'get-prototype-attribute parameters)))
                   (if (procedure? attribute)
                       (apply
                        (scope-method attribute current-object superclass attribute-name)
                        arguments)
                       attribute))))
            ('set-prototype-attribute
             (hashtable-set! prototype-attributes (car parameters) (cadr parameters)))
            ('new (object this-class superclass constructor parameters))
            ('super
             (let ((current-object (car parameters))
                   (constructor-params (cdr parameters)))
               (apply (scope-constructor constructor current-object superclass)
                      constructor-params)))
            ('type 'class))))
      this-class))

  (define (object class superclass constructor parameters)
    (let ((object-attributes (make-hash-table)))
      (define current-object
        (lambda (action . parameters)
          (case action
            ('get-attribute
             (let* ((attribute-name (car parameters))
                    (attribute (hashtable-ref object-attributes attribute-name (void)))
                    (arguments (cdr parameters)))
               (if (eq? attribute (void))
                   (apply class (cons* 'get-prototype-attribute current-object parameters))
                   (if (procedure? attribute)
                       (apply
                        (scope-method attribute current-object superclass attribute-name)
                        arguments)
                       attribute))))
            ('set-attribute
             (hashtable-set! object-attributes (car parameters) (cadr parameters)))
            ('type 'object))))
      (and (procedure? constructor)
           (apply (scope-constructor constructor current-object superclass)
                  parameters))
      current-object))

  (define-syntax new
    (syntax-rules ()
      ((_ class-name parameter ...)
       (class-name 'new parameter ...))))

  (define-syntax send
    (syntax-rules ()
      ((_ parent-name parameter rest ...)
       (parent-name (get-action parent-name) 'parameter rest ...))))

  (define (class? var)
    (eq? (var 'type) 'class))

  (define (object? var)
    (eq? (var 'type) 'object))

  (define (scope-constructor constructor current-object superclass)
    (constructor current-object
                 (lambda constructor-params
                   (apply superclass (cons* 'super current-object constructor-params)))))

  (define (scope-method method current-object superclass method-name)
    (method current-object
            (lambda method-params
              (apply superclass (cons* 'get-prototype-attribute
                                       current-object
                                       method-name
                                       method-params)))))

  (define (set-action parent)
    (cond ((class? parent) 'set-prototype-attribute)
          ((object? parent) 'set-attribute)))

  (define (get-action parent)
    (cond ((class? parent) 'get-class-attribute)
          ((object? parent) 'get-attribute))))
