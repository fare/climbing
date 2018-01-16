#lang at-exp racket @; -*- Scheme -*-
;; First-Class Implementations
;; 20 minute presentation at Off-the-Beaten-Track 2018-01-13
;; To compile it, use:
;;    racket fare-obt2018.rkt > fare-obt2018.html
;; This document is based on a previous talk:
;;    http://fare.tunes.org/computing/bal2009.ss
;;
;; This document is available under the bugroff license.
;;    http://www.oocities.org/soho/cafe/5947/bugroff.html

(require
 scribble/html
 net/url
 (for-syntax syntax/parse))

;; http://docs.racket-lang.org/scribble/extra-style.html

;; Reveal and new html stuff
(define/provide-elements/not-empty section video) ; more tags here

;; Register sections (but only at the top-level)
(define-values [get-sections register-section]
  (let ([sections '()])
    (values (λ () (reverse sections))
            (λ (section) (set! sections (cons section sections))))))
(define section-toplevel? (make-parameter #t))
(define-syntax-rule (slide (options ...) stuff ...)
  (do-slide (list options ...) (λ () (list stuff ...))))
(define (do-slide options thunk)
  (let ((toplevel? (section-toplevel?)))
    (parameterize ([section-toplevel? #f])
       (let ((section (apply section (append options (thunk)))))
         (if toplevel?
             (register-section section)
             section)))))
(define group-title (make-parameter #f))
(define-syntax-rule (slide-group title stuff ...)
  (do-slide-group title (λ () (list stuff ...))))
(define (do-slide-group title thunk)
  (slide ()
   (slide () @(h1 title))
   (parameterize ([group-title title])
     (thunk))))
(define (do-group-title)
  (when (group-title)
    (p align: 'right valign: 'top (font size: 4 (b (group-title))))))
(define-syntax-rule (gslide (options ...) stuff ...)
  (slide (options ...) (do-group-title) stuff ...))
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define (reveal-url . text)
  ;; (cons "http://cdn.jsdelivr.net/reveal.js/3.0.0/" text)
  (cons "resources/reveal/" text))

;; Quick helpers
(define-syntax-rule (defcodes lang ...)
  (begin (define (lang . text) (pre (code class: 'lang text)))
         ...))
(defcodes scheme javascript haskell)

;; Convert one digit to a roman numeral, given strings for one unit, five units and ten units.
(define (roman-numeral<-digit digit i v x)
  (case digit
    [(0) ""]
    [(1) i]
    [(2) (string-append i i)]
    [(3) (string-append i i i)]
    [(4) (string-append i v)]
    [(5) v]
    [(6) (string-append v i)]
    [(7) (string-append v i i)]
    [(8) (string-append v i i i)]
    [(9) (string-append i x)]
    [else (error "incorrect digit ~a" digit)]))

(define (roman-numeral<-integer n)
  ;; NB: works for integer from 1 to 3999
  (when (or (not (integer? n)) (< n 1) (> n 3999))
    (error "I cannot convert ~s to a roman numeral" n))
  (let* ((units (modulo n 10))
         (n/10 (/ (- n units) 10))
         (tens (modulo n/10 10))
         (n/100 (/ (- n/10 tens) 10))
         (hundreds (modulo n/100 10))
         (thousands (/ (- n/100 hundreds) 10)))
    (string-append
     (roman-numeral<-digit thousands "M" "" "")
     (roman-numeral<-digit hundreds "C" "D" "M")
     (roman-numeral<-digit tens "X" "L" "C")
     (roman-numeral<-digit units "I" "V" "X"))))

(define (LaTeX-name x)
  "LaTeX name for a symbol"
  (call-with-output-string
   (λ (out)
     (call-with-input-string
      (symbol->string x)
      (λ (in)
	(letrec
	    ((loop
	      (λ (upcase-next?)
		(let ((c (peek-char in)))
		  (cond
		   ((eof-object? c))
		   ((char-alphabetic? c)
		    (read-char in)
		    (display ((if upcase-next? char-upcase char-downcase) c) out)
		    (loop #f))
		   ((char-numeric? c)
		    (display #\x out)
		    (display (roman-numeral<-integer
			      (string->number
			       (bytes->string/latin-1
				(first
				 (regexp-match #rx"^[0-9]+" in)))))
			     out)
		    (loop #f))
		   (else
		    (read-char in)
		    (loop #t)))))))
	  (loop #f)))))))

(define (pic-url name url)
  (let ((file (string-append "resources/pic/" name)))
    (unless (file-exists? file)
      (define out (open-output-file file #:exists 'truncate))
      (call/input-url (string->url url)
                      get-pure-port
                      (λ (in) (copy-port in out)))
      (close-output-port out))
    file))

(define (L . x) (apply div align: 'left x))
(define (t . x) x)
(define (C . x) (apply div align: 'center x))
(define (CB . x) (C (apply b x)))

(define (url x) (a href: x (tt x)))
(define (comment . x) '())

(define (image name url . size)
  (img src: (pic-url name url) alt: name height: (if (empty? size) "75%" size)))
(define (fig name . options)
  (apply img src: (string-append "resources/pic/fig-" (LaTeX-name name) ".png")
         alt: name options))

(define *white* "#ffffff")
(define *gray* "#7f7f7f")
(define *blue* "#0000ff")
(define *light-blue* "#b4b4ff")
(define *red* "#ff0000")
(define *light-red* "#ffb4b4")
(define *green* "#00ff00")
(define *light-green* "#b4ffb4")

(define ~ @p{ })

(define (spacing* l (space (br)))
  (cond
    ((null? l) (list space))
    ((pair? l) (append (list space)
                       (if (pair? (car l)) (car l) (list (car l)))
                       (spacing* (cdr l))))
    (else (error 'spacing*))))

(define (spacing l)
  (if (list? l)
      (cdr (spacing* (filter-not null? l)))
      l))

(define (color text #:fg (fgcolor #f) #:bg (bgcolor #f))
  (if (or fgcolor bgcolor)
      (span style: (list (if fgcolor (list "color:" fgcolor ";") '())
                     (if bgcolor (list "background-color:" bgcolor ";") '()))
            text)
      text))

(define (gray . text) (color text #:fg *gray*))

(define (bg-slide text fgcolor bgcolor)
  (λ x
    (gslide (data-background: bgcolor)
     (spacing x)
     (div align: 'right valign: 'bottom (color #:fg fgcolor text)))))

(define-syntax-rule (x-slide (options ...) x ...)
  (gslide (options ...) (spacing (list x ...))))

;;(define th-width "4%")
;;(define td-width "48%")
;;(define table-width "114%")
(define th-width "8%")
(define td-width "46%")
(define table-width "104%")

(define (th* name)
  (if name (th width: th-width (font size: "6" (color name #:fg *white*))) (td)))

(define (row name left right
             #:left-bg (left-bg #f) #:right-bg (right-bg #f) #:fragment? (fragment? #f))
  (tr
   (th* name)
   (td
    width: td-width (when left-bg bgcolor:) (when left-bg left-bg)
    (spacing left))
   (if right
       (td
        width: td-width bgcolor: right-bg
        (when fragment? class:) (when fragment? 'fragment)
        (when fragment? data-fragment-index:) (when fragment? 1)
        (spacing right))
       (td width: td-width))))


(slide ()
 @h1{Climbing Up the Semantic Tower - at Runtime}
 ~
 @bt{First Class Implementations}
 ~
 ~
 ~
 @p{François-René Rideau, @em{Metafore Hyperpost}}
 ~
 ~
 @p{Off the Beaten Track 2018, 2018-01-13}
 @url{http://github.com/fare/climbing})

(slide-group "Semantic Towers"
(gslide (data-transition: 'none)
  (fig 'simple-tower-o height: "600"))
(gslide (data-transition: 'none)
  (fig 'simple-tower-1 height: "600"))
(gslide (data-transition: 'none)
  (fig 'simple-tower-2 height: "600"))
(gslide (data-transition: 'none)
  (fig 'simple-tower-3 height: "600"))
(gslide (data-transition: 'none)
  (fig 'simple-tower-4 height: "600"))
(gslide (data-transition: 'none)
  (fig 'simple-tower-5 height: "600"))
(gslide (data-transition: 'none)
  (fig 'simple-tower-6 height: "600")))

(slide-group "Conclusion"
(gslide ()
 @h1{The Grand Challenge}
 ~
 @L{None of these Stories is revolutionary} ;; From The Mother of All Demos...
 @L{Each has been foretold in past systems} ;; Implemented, though not always optimized and productized
 ~
 (div class: 'fragment
  @L{Yet no @em{system} embodies them all at once} ;; Opportunity!
  @L{Missing: not technical ability, but @em{vision}})

 @comment{
   We have the opportunity to do so much better!
 })

(x-slide ()
 @h1{The Take Home Points (redux)}
 @L{Stories @em{matter}} @comment{Don't let yourself be an NPC in a bad story.}
 @L{Software tools imply a story, and @em{vice versa}} @comment{like a Fourier Transform}
 @L{Better tools via better stories} @comment{if you stick to them}
 @L{Explicit stories as great meta-tool...} @comment{})


(slide
 @h1{The Meta-Story}
 @p[class: 'fragment]{
   Foo
 }
 @p[class: 'fragment]{
   @br[]
   @cite{Efficiency is doing things right; effectiveness is doing the right things.}
   — Peter Drucker
 }
 ;; ~ @p[class: 'fragment]{Any question?}
 ))

#| Submission to LambdaConf 2017:

LambdaConf 2017 - Call for Proposals
Inspire Session (10 minutes)
Thanks for your interest in leading an Inspire Session at LambdaConf 2017! Please answer these questions as best you can. While you can always make tweaks after your proposal has been accepted, the determination of whether or not to include your proposal will be based on the answers you provide now.

* 3 Title. What is the title of your proposal?

Better Stories, Better Software

* 4 Introduction. What is this session about?

I will present several pairs of stories about how and why software is written, and what adverse or positive effects they have on what software is written.

While my personal opinion about which story of each a pair is better than the other may be controversial — the fact that some stories have vast effects opposite to each other will hopefully not be.

* 5 Takeaway. What is the ONE takeaway for developers who attend your session?

The structure of software is implied by the stories we tell. To build better software, tell better stories.

* 6 Inspiration. In what way do you hope your session will inspire developers?

I will inspire developers to think not just about the formal structure of software, inside the computer, but also about the informal interactions of which the software is part of, involving humans.

7 Entertainment. If relevant, in what way do you hope your session will entertain developers?

Developers will be entertained by realizing that a lot of the frustration they experience can be summarized in silly stories about software.


* 8 Relevancy. Why is this session relevant to a professional software developer?

Professional software developer sometimes need to step back and think about what they are doing, whether they should keep going one way, and if not, what to do next. At those crucial moments, perspective is crucial. I hope to contribute to such perspective.

* 9 Benefits. How will the subject matter you're covering help developers to better accomplish their job?

"Efficiency is doing things right; effectiveness is doing the right things." — Peter Drucker

I'm hoping to help with effectiveness, not efficiency.


* 10 Outline. Please create a brief outline how you intend to structure the session.

1- "Easy" stories that everyone is familiar with, and how they change the shape of software (e.g. free software vs proprietary software)

* 11 Pitch. What is the main reason developers should come to your session instead of other ones?

Stories are fun. Stories have consequences. Don't let yourself be a NPC in a bad Story.

* 12 Background Requirements. If your session is on statically-typed, category-theoretic functional programming (Haskell, PureScript, Scala, etc.), please choose the category that best matches the contents of your session, such that people who are actively learning or mostly know the category contents will understand your session.

Note: These topic categories are based on LOFP—please see here for more details.

The session is not related to statically-typed, category-theoretic families (Haskell, PureScript, Scala)

13 If relevant, what language(s) will you use to provide code samples?

N/A
|#

(output-xml
 @html{
   @head{
     @meta[http-equiv: 'content-type content: "text/html; charset=utf-8"]
     @link[rel: 'stylesheet href: "resources/my.css"]
     @link[rel: 'stylesheet href: @reveal-url{css/reveal.css}]
     @link[rel: 'stylesheet href: @reveal-url{css/theme/black.css}]
     @link[rel: 'stylesheet href: @reveal-url{lib/css/zenburn.css}]
     @link[rel: 'stylesheet href: "resources/my.css"]
   }
   @body{
     @div[class: 'reveal]{@div[class: 'slides]{@get-sections}}
     @script[src: @reveal-url{lib/js/head.min.js}]
     @script[src: @reveal-url{js/reveal.min.js}]
     @script/inline{
       Reveal.initialize({
         dependencies: [
           {src: "@reveal-url{plugin/highlight/highlight.js}",
            async: true, callback: () => hljs.initHighlightingOnLoad()}],
         controls: false,
         // transition: none
       });
     }}})

@; http://www.michaelnygard.com/blog/2017/11/root-cause-analysis-as-storytelling/
