
MAKEINFO	= $(shell which makeinfo 2> /dev/null)
DVIPS		= $(shell which dvips 2> /dev/null)
TEXI2DVI	= $(shell which texi2dvi 2> /dev/null)
PDFTEX		= $(shell which pdftex 2> /dev/null)

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all tagged abstract renames \
		pragma new case when null
build_doc: gwiad.texi gwiad.pdf gwiad.html gwiad.ps gwiad.txt gwiad.info
	echo ""
	echo Gwiad Documentation built with success.

%.dvi: %.texi
ifneq (${TEXI2DVI},)
	echo Building $@
	-${TEXI2DVI} --expand --clean --quiet $<
else
	@echo "--------------------------------------------------------"
	@echo "texi2dvi not found, cannot build DVI or PS documentation"
	@echo "--------------------------------------------------------"
endif

%.ps: %.dvi
ifneq (${DVIPS},)
	echo Building $@
	-${DVIPS} -q $< -o $@
else
	@echo "------------------------------------------------------"
	@echo "dvips not found, cannot build POSTSCRIPT documentation"
	@echo "------------------------------------------------------"
endif

%.pdf: %.texi
ifneq (${TEXI2DVI},)
ifneq (${PDFTEX},)
	echo Building $@
	${TEXI2DVI} -p --expand --clean --quiet $<
else
	@echo "------------------------------------------------"
	@echo "pdftex not found, cannot build PDF documentation"
	@echo "------------------------------------------------"
endif
else
	@echo "--------------------------------------------------"
	@echo "texi2dvi not found, cannot build PDF documentation"
	@echo "--------------------------------------------------"
endif

%.info: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build INFO documentation"
	@echo "---------------------------------------------------"
endif

%.html: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --html --no-split --css-include=gwiad.css --ifinfo $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build HTML documentation"
	@echo "---------------------------------------------------"
endif

%.txt: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --plaintext --no-headers $< --output $@
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build TEXT documentation"
	@echo "---------------------------------------------------"
endif

gwiad.texi: gen_doc.sed gwiad.texi.tmplt
	echo build from script
	sed -f ./gen_doc.sed < gwiad.texi.tmplt > gwiad.texi

gen_doc.sed:
	gnatmake -P docs.gpr
	./build

clean:
	-$(RM) -f gwiad.texi gwiad.dvi gwiad.html gwiad.info* \
	gwiad.log gwiad.ps gwiad.pdf gwiad.txt gen_doc.sed
	gnatclean -Pdocs
