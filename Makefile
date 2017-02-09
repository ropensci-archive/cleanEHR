all:
	Rscript -e 'library(methods); library(roxygen2); roxygenize(".")'
	R CMD INSTALL .
#	Rscript install.r man

check:
	R CMD check '.'

doc:
	Rscript -e "library(devtools); build_vignettes()"

cran: 
	if [ -d cran_cleanEHR ]; then  rm -r cran_cleanEHR; fi 
	mkdir cran_cleanEHR 
	cp -r R man data inst src tests DESCRIPTION NAMESPACE vignettes cran_cleanEHR
	rm cran_cleanEHR/src/*.o cran_cleanEHR/src/*.so
	R CMD build cran_cleanEHR 
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
	rsync -av . /tmp/cleanEHR --exclude '.*' --exclude '*.so' --exclude '*.o'
	zip -r ../cleanEHR.zip /tmp/cleanEHR
	rm -r /tmp/cleanEHR
