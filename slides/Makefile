all: slides.pdf

slides.pdf: slides.lhs
	lhs2TeX -o slides.tex slides.lhs	
	latexmk -pdf -xelatex -shell-escape -use-make slides.tex
clean:
	latexmk -CA