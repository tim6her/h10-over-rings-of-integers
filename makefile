pdf:
	latexmk --xelatex --synctex=1 Herbstrith-H10_over_AI

html:
	pdf2htmlEX Herbstrith-H10_over_AI.pdf docs/index.html
