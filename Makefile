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
	evince -f -i $${p:-1} $<
	#xpdf -z page -fullscreen $< :$${p:-1}

%.pdf: %.scrbl ${src}
	RENDER_MODE=latex time scribble --dest-name $@ --pdf $<

pngs:
	-for i in boundedLiveness observability soundness completeness ; do \
	  [ -f fig-$$i.png ] || \
	  convert -density 600x600 -antialias -background white -flatten fig-$$i.pdf fig-$$i.png ; \
	done

${ae}.html: ${ae}.scrbl ${src}
%.html: %.scrbl utils.rkt bibliography.scrbl pngs
	RENDER_MODE=html time scribble --dest-name $@ --html $<

%.latex: %.scrbl ${src}
	RENDER_MODE=latex time scribble --latex --dest build $<

clean:
	rm -f *.pdf *.html *.tex *.css *.js
	rm -rf tmp

mrproper:
	git clean -xfd

rsync: climbing.pdf climbing.html
	rsync -av --delete $^ *.js *.css *.png ~/files/climbing/
	rsync -av --delete ~/files/climbing/ bespin:files/climbing/

fare-obt2018.html: fare-obt2018.rkt
	racket $< > $@

fci: fci-obt2018.ss
	slideshow --start $${p:-1} $< # --comment-on-slide

%.pdf: %.ss
	slideshow --pdf -o $@ $<

fci-obt2018.pdf:

link:
	ln -s /home/fare/fare/phdthesis/build resources/pic
