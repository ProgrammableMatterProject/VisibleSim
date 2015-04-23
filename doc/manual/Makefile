all: manual.pdf

manual.pdf: manual.tex manual.bib 
	pdflatex manual.tex
	pdflatex manual.tex
	bibtex manual
	pdflatex manual.tex
	pdflatex manual.tex

clean:
	rm -f *.toc *.log *.aux *.bbl *.blg *.out *.brf *.synctex.gz

reallyclean: clean
	rm -f manual.pdf
