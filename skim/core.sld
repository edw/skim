;;; -*- mode: Scheme -*-
;;;
;;; Skim is a Scheme dialect that attempts to embody many of the
;;; lessons of Clojure while retaining Scheme's semantics--and,
;;; importantly, its reader.
;;;
;;; Edwin Watkeys, edw@poseur.com
;;; May 2016

(define-library (skim core)
  (export fn defn lit lip cind
          cursor cursor-next! cursor-null? cursor-value
          fir dosiq -> -->)
  (import (scheme base)
          (chibi match))
  (begin
    (define-syntax fn
     (syntax-rules ()
       ((_ (#(pat-elem ...) . body) ...)
        (lambda expr (match expr ((pat-elem ...) . body) ...)))

       ((_ #(pat-elem ...) . body)
        (lambda expr (match expr ((pat-elem ...) . body))))

       ((_ docstring (#(pat-elem ...) . body) ...)
        (lambda expr docstring (match expr ((pat-elem ...) . body) ...)))

       ((_ docstring #(pat-elem ...) . body)
        (lambda expr docstring (match expr ((pat-elem ...) . body))))))

   (define-syntax defn
     (syntax-rules ()
       ((_ name (#(pat-elem ...) . body) ...)
        (define name (fn (#(pat-elem ...) . body) ...)))

       ((_ name #(pat-elem ...) . body)
        (define name (fn #(pat-elem ...) . body)))

       ((_ name docstring (#(pat-elem ...) . body) ...)
        (define name (fn docstring (#(pat-elem ...) . body) ...)))

       ((_ name docstring #(pat-elem ...) . body)
        (define name (fn docstring #(pat-elem ...) . body)))))

   (define-syntax lit
     (syntax-rules ()

       ((_ #() . body)
        (begin . body))

       ((_ #(name value name2 ...) . body)
        (let ((name value)) (lit #(name2 ...) . body)))))

   (define-syntax bindings-lambda
     (syntax-rules ()

       ((_ () body params)
        (lambda params . body))

       ((_ (name1 value1 name2 ...) body (param ...))
        (bindings-lambda (name2 ...) body (param ... name1)))))

   (define-syntax bindings-apply
     (syntax-rules ()

       ((_ proc () values)
        (proc . values))

       ((_ proc (name1 value1 name2 ...) (value ...))
        (bindings-apply proc (name2 ...) (value ... value1)))))

   (define-syntax lip
     (syntax-rules ()
       ((_ proc #(binding ...) . body)
        (letrec ((proc (bindings-lambda (binding ...) body ())))
          (bindings-apply proc (binding ...) ())))))

   (define-syntax cind
     (syntax-rules (else)
       ((_ else form) form)
       ((_ test form . tfs) (if test form (cind . tfs)))))

   (define (cursor col)
     (cond ((list? col)
            (letrec ((proc
                      (lambda (method)
                        (cond ((equal? method 'value)
                               (if (null? col)
                                   '()
                                   (car col)))
                              ((equal? method 'next)
                               (if (not (null? col))
                                   (set! col (cdr col)))
                               proc)
                              ((equal? method 'null?)
                               (null? col))))))
              proc))
           ((vector? col)
            (let ((index 0)
                  (count (vector-length col)))
              (letrec ((proc
                        (lambda (method)
                          (cond ((equal? method 'value)
                                 (if (< index count)
                                     (vector-ref col index)
                                     '()))
                                ((equal? method 'next)
                                 (if (< index count)
                                     (set! index (+ index 1)))
                                 proc)
                                ((equal? method 'null?)
                                 (not (< index count)))))))
                proc)))
           (else (error "Unsupported type"))))

   (define (cursor-value c) (c 'value))
   (define (cursor-next! c) (c 'next))
   (define (cursor-null? c) (c 'null?))

   (define-syntax fir
     (syntax-rules (lit: when: while:)

       ((_ #() form outer-iter outer-in outer-out)
        (outer-iter (cursor-next! outer-in) (cons form outer-out)))

       ((_ #(while: pred-form name ...) form outer-iter outer-in outer-out)
        (if pred-form
            (fir #(name ...) form outer-iter outer-in outer-out)
            (reverse outer-out)))
      
       ((_ #(when: pred-form name ...) form outer-iter outer-in outer-out)
        (if pred-form
            (fir #(name ...) form outer-iter outer-in outer-out)
            (outer-iter (cursor-next! outer-in) outer-out)))

       ((_ #(lit: #(binding ...) name ...) form outer-iter outer-in outer-out)
        (lit #(binding ...)
             (fir #(name ...) form outer-iter outer-in outer-out)))

       ((_ #(name1 values1 name2 ...) form outer-iter outer-in outer-out)
        (lip iter #(in (cursor values1) out outer-out)
             (if (cursor-null? in) (outer-iter (cursor-next! outer-in) out)
                 (lit #(name1 (cursor-value in))
                      (fir #(name2 ...) form iter in out)))))

       ((_ #(binding ...) form)
        (fir #(binding ...)
             form
             (lambda (_ out) (reverse out))
             (cursor '())
             '()))))

   (define-syntax dosiq
     (syntax-rules (lit: when: while:)

       ((_ #() form outer-iter outer-in)
        (begin form
               (outer-iter (cursor-next! outer-in))))

       ((_ #(while: pred-form name ...) form outer-iter outer-in)
        (if pred-form
            (dosiq #(name ...) form outer-iter outer-in)))
      
       ((_ #(when: pred-form name ...) form outer-iter outer-in)
        (if pred-form
            (dosiq #(name ...) form outer-iter outer-in)
            (outer-iter (cursor-next! outer-in))))

       ((_ #(lit: #(binding ...) name ...) form outer-iter outer-in)
        (lit #(binding ...)
             (dosiq #(name ...) form outer-iter outer-in)))

       ((_ #(name1 values1 name2 ...) form outer-iter outer-in)
        (lip iter #(in (cursor values1))
             (if (cursor-null? in) (outer-iter (cursor-next! outer-in))
                 (lit #(name1 (cursor-value in))
                      (dosiq #(name2 ...) form iter in)))))

       ((_ #(binding ...) form)
        (dosiq #(binding ...)
             form
             (lambda (_) (values))
             (cursor '())))))

   (define-syntax ->
     (syntax-rules ()
       ((_ value)
        value)
       ((_ value (proc arg ...))
        (-> (proc value arg ...)))
       ((_ value (proc arg ...) (proc2 ...))
        (-> (proc value arg ...) (proc2 ...)))))

   (define-syntax -->
     (syntax-rules ()
       ((_ value)
        value)
       ((_ value (proc arg ...))
        (--> (proc arg ... value)))
       ((_ value (proc arg ...) (proc2 ...))
        (--> (proc arg ... value) (proc2 ...)))))))


