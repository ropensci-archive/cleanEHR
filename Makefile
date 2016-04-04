all:
	Rscript install.r man
check:
	R CMD check '.'
test:
	@R -e 'library(devtools); test()'
