makeall:
	make makedev
	make clean

makedev:
	make amba.pdf

amba.pdf: amba.tex maia.sty
	pdflatex amba.tex

amba.tex: amba.rnw
	R CMD Sweave amba.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log
	rm tmp*
	rm Rplots*

