html:
	pandoc -t html -s -f markdown --toc --template="./templates/pandoc-amsthm.html5" --mathjax --filter pandoc-crossref --filter pandoc-citeproc *.md --css pandoc.css -o docs/index.html
