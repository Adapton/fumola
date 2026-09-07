# The report is a PDF to print, so the PDF is committed beside its source.
# XCharter needs nothing but a TeX Live with the xcharter package.
repair-cost.pdf: repair-cost.tex $(wildcard data/*.csv)
	pdflatex -interaction=nonstopmode -halt-on-error repair-cost.tex
	pdflatex -interaction=nonstopmode -halt-on-error repair-cost.tex

clean:
	rm -f *.aux *.log *.out *.toc

.PHONY: clean
