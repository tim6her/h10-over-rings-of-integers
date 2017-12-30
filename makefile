html:
	pandoc -t html -s -f markdown --template="/Users/timbenediktherbstrith/Documents/Projects/pandoc-amsthm/template/pandoc-amsthm.html5" --mathjax --filter pandoc-citeproc *.md --css pandoc.css -o docs/index.html
