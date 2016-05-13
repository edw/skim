(use-modules (ice-9 match)) ;; Guile
;; (require-extension matchable) ;; Chicken

;; Skim is a Scheme dialect that attempts to embody many of the
;; lessons of Clojure while retaining Scheme's semantics--and,
;; importantly, its reader.

;; Edwin Watkeys, edw@poseur.com
;; May 2016

(define-syntax fn
  (syntax-rules ()
    ((_ (pattern . body) ...)
     (lambda expr (match expr (pattern . body) ...)))))

(define-syntax defn
  (syntax-rules ()
    ((_ name (pattern . body) ...)
     (define name (fn (pattern . body) ...)))))

(define-syntax lit
  (syntax-rules ()

    ((_ () . body)
     (begin . body))

    ((_ (name value . nvs) . body)
     (letrec ((name value)) (lit nvs . body)))))

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
    ((_ proc bindings . body)
     (lit (proc (bindings-lambda bindings body ()))
       (bindings-apply proc bindings ())))))

(define-syntax cind
  (syntax-rules (else)
    ((_ else form) form)
    ((_ test form . tfs) (if test form (cind . tfs)))))

(define-syntax fir
  (syntax-rules (lit:)

    ((_ () form)
     form)

    ((_ (lit: bindings name2 ...) form)
     (lit bindings (fir (name2 ...) form)))

    ((_ (name1 values1 name2 ...) form)
     (map (lambda (name1) (fir (name2 ...) form)) values1))))

(define-syntax fir
  (syntax-rules (lit: when: while:)

    ((_ () form iter in out)
     (iter (cdr in) (cons form out)))

    ((_ (while: pred-form name ...) form iter in out)
     (if pred-form
         (fir (name ...) form iter in out)
         (reverse out)))
    
    ((_ (when: pred-form name ...) form iter in out)
     (if pred-form
         (fir (name ...) form iter in out)
         (iter (cdr in) out)))

    ((_ (lit: bindings name ...) form iter in out)
     (lit bindings (fir (name ...) form iter in out)))

    ((_ (name1 values1 name2 ...) form iter in out)
     (lip iter2 (in2 values1 out2 out)
          (if (null? in2) (iter (cdr in) out2)
              (lit (name1 (car in2))
                   (fir (name2 ...) form iter2 in2 out2)))))

    ((_ (name1 values1 name2 ...) form)
     (lip iter (in values1 out '())
          (if (null? in) (reverse out)
              (lit (name1 (car in))
                   (fir (name2 ...) form iter in out)))))))

