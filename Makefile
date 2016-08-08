all:
	Rscript install.r man
check:
	R CMD check '.'
test:
	@Rscript -e 'library(devtools); test()'

clean:
	rm -rf src/*.o src/*.so
	rm -rf man
