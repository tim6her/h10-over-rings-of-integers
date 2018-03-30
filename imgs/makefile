all:
	bash pdf2svg.sh
	cp *.pdf ../res/
	cp *.svg ../res/
	mv *.svg ../docs/res/
	cp *.png ../res/
	cp *.png ../docs/res/


pdf:
	latexmk -xelatex *.tex
