all:
	Rscript -e 'library(methods); library(roxygen2); roxygenize(".")'
	R CMD INSTALL .
#	Rscript install.r man

check:
	R CMD check '.'
test:
	@Rscript -e 'library(devtools); test()'

manual:
	R CMD Rd2pdf . --force

clean:
	rm -rf src/*.o src/*.so
	rm -rf man
