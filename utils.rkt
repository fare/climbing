#lang at-exp racket
;;-*- Scheme -*-

(require
  scribble/base
  scribble/bnf
  scribble/core
  scribble/decode
  scribble/manual
  scriblib/autobib
  scriblib/figure
  scriblib/footnote
  scriblib/render-cond
  ;;(only-in scribble/acmart acmart-style)
  (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax-rule (NOP _ ...) (begin))

(define (note-url url)
  (note (hyperlink url url)))

(define extended-url "http://fare.tunes.org/files/asdf3/asdf3-2014.html")

(define (sf . str) (make-element 'sf (decode-content str)))

(define backend (make-parameter '#:html))
(define-syntax-rule (html-only x ...) (and (eq? (backend) '#:html) (list x ...)))
(define-syntax-rule (pdf-only x ...) (and (eq? (backend) '#:pdf) (list x ...)))

(define multiple-sections (make-parameter #f))

(define (moneyquote . x) (bold x))
(define (q . x) (list "``" x "''"))

(define-syntax (Lblock stx)
  (syntax-parse stx
    [(_ #:line-numbers ln str ...)
     #'@codeblock[;;#:keep-lang-line? #f
                   #:line-numbers ln
                   #:line-number-sep 3
                   str ...]]
    [(_ str ...)
     #'(clblock #:line-numbers 0 str ...)]))

(define-syntax (Lcode stx)
  (syntax-parse stx
    [(_ str ...) #'(Lblock #:line-numbers #f str ...)]))

(define (L . str) (apply tt str))

(define (CommonLisp) "Common Lisp")

(define-syntax-rule (XXX . rest) '())
(define (latin x) (emph x))
(define (ad_hoc) @latin{ad hoc})
(define (de_facto) @latin{de facto})
(define (bydef . x) (emph x))

(define-syntax defpretty
  (lambda (stx)
    (syntax-case stx ()
      [(_ pretty name ...)
       (with-syntax ([(string ...) (map symbol->string (syntax->datum #'(name ...)))])
         #'(begin
             (define (name) (pretty string)) ...))])))

;;(defpretty)

;;(defpretty emph depends-on in-order-to do-first force)
;;(defpretty tt Make make blaze)

(define-cite ~cite cite-noun generate-bib #:style number-style) ;; acmart-style

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))

(define (Phi) "Φ")
(define (phi) "φ")
(define (Psi) "Ψ")
(define (psi) "ψ")
(define (circ) "∘")

(define (raw-latex . args)
  (element (style "relax" '(exact-chars)) args))

(define (spacing)
  (element-with-render-mode
   (λ (mode)
     (case mode
       [(html) " "]
       [(latex) (raw-latex "~~~~~\\hfill~~~~~~")]))))

(define (element-with-render-mode f)
  (cond-element
    [html (f 'html)]
    [latex (f 'latex)]
    ;;[text (f 'text)]
    [else (error "Unsupported render mode")]))

(define (block-with-render-mode f)
  (cond-block
    [html (f 'html)]
    [latex (f 'latex)]
    ;;[text (f 'text)]
    [else (error "Unsupported render mode")]))

(define (separate-list sep l)
  (if (or (null? l) (null? (cdr l))) l
      (cons (car l) (cons sep (separate-list sep (cdr l))))))

(define (image-type mode)
  (case mode
    [(html) "png"]
    [(latex) "pdf"]))


(define (make-figure-table ps figure-dir)
  (block-with-render-mode
   (λ (mode)
     (let ([image-path (λ (x) (format "~a/fig-~a" figure-dir (car x)))]
           [scale (case mode [(html) 1/3] [(latex) .55])])
       (tabular
        #:style 'center
        #:column-properties (make-list (length ps) 'center)
        (list
         (separate-list
          (spacing)
          (map (λ (x)
                 (image #:suffixes '(".pdf" ".png") #:scale scale (image-path x)))
               ps))
         (separate-list
          (spacing)
          (map (λ (x) (centered (cadr x))) ps))))))))

(define m L)

(define (TODOcite) "[?]")
