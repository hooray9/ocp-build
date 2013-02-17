all: user-manual.pdf

SOURCES=user-manual.tex content.tex

user-manual.pdf: $(SOURCES)
	pdflatex user-manual
	pdflatex user-manual

view: user-manual.pdf
	evince user-manual.pdf
