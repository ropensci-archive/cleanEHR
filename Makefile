all:
	Rscript -e 'library(methods); library(roxygen2); roxygenize(".")'
	R CMD INSTALL .
#	Rscript install.r man

check:
	R CMD check '.'

doc:
	Rscript -e "library(devtools); build_vignettes()"

cran: 
	if [ -d cran_ccdata ]; then  rm -r cran_ccdata; fi 
	mkdir cran_ccdata 
	cp -r R man data inst src tests DESCRIPTION NAMESPACE vignettes cran_ccdata
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

rmcran: 
	rm -rf cran*
	rm -rf *.Rcheck
	rm *.tar.gz

idhs:
	rsync -av . /tmp/ccdata --exclude '.*' --exclude '*.so' --exclude '*.o'
	zip -r ../ccdata.zip /tmp/ccdata
	rm -r /tmp/ccdata
