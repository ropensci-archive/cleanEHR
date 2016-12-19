all:
	Rscript -e 'library(methods); library(roxygen2); roxygenize(".")'
	R CMD INSTALL .
#	Rscript install.r man

check:
	R CMD check '.'

cran:
	rm -r cran_ccdata
	mkdir cran_ccdata 
	cp -r R man data inst src tests DESCRIPTION NAMESPACE cran_ccdata
	rm cran_ccdata/src/*.o cran_ccdata/src/*.so
	R CMD build cran_ccdata 
	R CMD check *.tar.gz --as-cran 

test:
	@Rscript -e 'library(devtools); test()'

manual:
	R CMD Rd2pdf . --force

clean:
	rm -rf src/*.o src/*.so
	rm -rf man
	rm ..pdf
	rm *.tar.gz
	rm ccdata.*
	rm ccdata_*

idhs:
	rsync -av . /tmp/ccdata --exclude '.*' --exclude '*.so' --exclude '*.o'
	zip -r ../ccdata.zip /tmp/ccdata
	rm -r /tmp/ccdata
