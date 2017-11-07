ae := climbing

src = climbing.scrbl utils.rkt bibliography.scrbl
# asdf3.scrbl old-bug.scrbl history.scrbl tutorial.scrbl

#export PLTCOLLECTS:=$(shell pwd):${PLTCOLLECTS}

all: climbing.PDF
ann: climbing-annotated.PDF
html: ${ae}.html
pdf: ${ae}.pdf
PDF: pdf ${ae}.PDF

install: climbing.html climbing.pdf
	rsync -av --delete $^ *.js *.css ~/files/climbing/
	rsync -av --delete ~/files/climbing/ bespin:files/climbing/

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	donuts.pl unhtml < $< | wc

%.PDF: %.pdf
	#evince -f -i $${p:-1} $<
	xpdf -z page -fullscreen $< :$${p:-1}

%.pdf: %.scrbl ${src}
	time scribble --dest-name $@ --pdf $<

${ae}.html: ${ae}.scrbl ${src}
%.html: %.scrbl utils.rkt bibliography.scrbl
	time scribble --dest-name $@ --html $<

%.latex: %.scrbl ${src}
	time scribble --latex --dest tmp $<

clean:
	rm -f *.pdf *.html *.tex *.css *.js
	rm -rf tmp

mrproper:
	git clean -xfd

rsync: html pdf
	rsync -av ${ae}.html ${ae}.pdf common-lisp.net:~frideau/public_html/climbing/

