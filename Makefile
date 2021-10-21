build:
	git add .
	git commit
	R --slave -e 'devtools::document(); styler::style_dir("."); pkgdown::build_site()'
	git add .
	git commit -m "Documentation & styling"
	git push
	R CMD BUILD .
	R CMD INSTALL .

install:
	R CMD BUILD .
	R CMD INSTALL .
	
