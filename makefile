##############################################################################
##                                Gwiad                                     ##
##                                                                          ##
##                        Copyright (C) 2007-2008                           ##
##                            Olivier Ramonat                               ##
##                                                                          ##
##  This library is free software; you can redistribute it and/or modify    ##
##  it under the terms of the GNU General Public License as published by    ##
##  the Free Software Foundation; either version 2 of the License, or (at   ##
##  your option) any later version.                                         ##
##                                                                          ##
##  This library is distributed in the hope that it will be useful, but     ##
##  WITHOUT ANY WARRANTY; without even the implied warranty of              ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       ##
##  General Public License for more details.                                ##
##                                                                          ##
##  You should have received a copy of the GNU General Public License       ##
##  along with this library; if not, write to the Free Software Foundation, ##
##  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       ##
##############################################################################

include mk.config

ifeq ($(OS),Windows_NT)
SOEXT=.dll
EXEEXT=.exe
else
SOEXT=.so
EXEEXT=
endif

APP := $(ADA_PROJECT_PATH)

ifeq ($(OS),Windows_NT)
export ADA_PROJECT_PATH=$PWD/external-libs/morzhol\;${APP}
else
export ADA_PROJECT_PATH=$PWD/external-libs/morzhol:${APP}
endif

export LIBRARY_TYPE=relocatable

# Set BDIR to .build/#lowercase_mode#
BDIR = .build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])

GENERAL_OPTIONS = CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" \
	GNATMAKE="$(GNATMAKE)" GNATCLEAN="$(GNATCLEAN)" \
	GNATCHECK="$(GNATCHECK)" GNATCHOP="$(GNATCHOP)" \
	EXEEXT="$(EXEEXT)" DIFF="$(DIFF)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)

# Modules support

MODULES =  external-libs gwiad dynamic_libraries plugins web admin \
		example argwiad argwiadctl

all: build

include mk.modules

# Version

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

# Targets

mkinstall: force
ifneq ($(INSTALL), "")
# Write INSTALL target into mk.install (see install target)
	$(shell echo INSTALL = $(INSTALL) > mk.install)
endif

build: mkinstall build-default

clean: clean-default
	make -C regtests clean $(OPTIONS)

clean-all:
	$(RM) -f mk.install
	$(RM) -r .build

setup: setup-version setup-default

setup-version:
# If git is not present then use the version.ads provided in distrib
ifneq ("$(VERSION)", "")
	sed -e 's,\$$VERSION\$$,$(VERSION),g' \
	-e 's,\$$VERSION_ALL\$$,$(VERSION_ALL),g' \
	gwiad/src/gwiad-version.tads > gwiad/src/gwiad-version.ads
endif

distrib:
	git archive --prefix=gwiad/ HEAD > gwiad.tar
	tar -C ../ -r --file=gwiad.tar gwiad/gwiad/src/gwiad-version.ads
	gzip -f gwiad.tar

regtests: force
	make -C external-libs/morzhol regtests \
		MODE="Profile" $(GENERAL_OPTIONS)
	make -C regtests MODE="Profile" BDIR=".build/profile" $(GENERAL_OPTIONS)
	rm -f regtests/obj/*	# To avoid error in lcov_analyse ???
	make lcov_analyse

lcov_analyse: force
	sh analyse.sh

build_doc:
	@echo ""
	@echo "=== Build doc"
	${MAKE} -C doc build_doc $(GALL_OPTIONS)
	@echo ""
	@echo "=== Build argwiadctl manpage"
	${MAKE} -C argwiadctl build_doc

check :	check_message check-default
	@echo
	@echo "#####################################"
	@echo "### Check style with style_checker ##"
	@echo "#####################################"
	@echo
	-find . -follow -not -name "b~*" \
		-a \( -name "*.adb" -o -name "*.ads" \) \
		-and -not -path "*tsrc*" \
		| xargs style_checker -lang Ada -cp -cy -sp -gnat05

check_message:
	@echo
	@echo "############################################"
	@echo "### Verifying Properties Using gnatcheck ###"
	@echo "############################################"
	@echo

force:

ifeq ($(OS),Windows_NT)
DISTRIB_OS = Windows
else
DISTRIB_OS = $(shell uname -s | tr [[:upper:]] [[:lower:]])-$(shell uname -m)
endif

DISTRIB = argwiad-$(DISTRIB_OS)-$(VERSION_ALL)

install_demo: install_server
	$(CP) -r $(BDIR)/slib/services/*hello_world_service$(SOEXT) \
		$(SERVER_INSTALL)/lib/services/
	$(CP) -r $(BDIR)/slib/websites/*hello_world_website$(SOEXT) \
		$(SERVER_INSTALL)/lib/websites/
	$(CP) $(BDIR)/lib/*helloworld_interface*$(SOEXT) $(SERVER_INSTALL)/bin

install_admin_plugin:
	$(CP) -r admin/templates/*.thtml \
		$(SERVER_INSTALL)/templates/admin
	$(CP) -r $(BDIR)/slib/websites/libgwiad_admin$(SOEXT) \
		$(SERVER_INSTALL)/lib/websites/

install_server:
	$(MKDIR) $(SERVER_INSTALL)/data
	$(MKDIR) $(SERVER_INSTALL)/bin
	$(RM) -r $(SERVER_INSTALL)/lib/
	$(MKDIR) $(SERVER_INSTALL)/lib/websites
	$(MKDIR) $(SERVER_INSTALL)/lib/services
	$(MKDIR) $(SERVER_INSTALL)/templates
	$(MKDIR) $(SERVER_INSTALL)/templates/admin
	$(MKDIR) $(SERVER_INSTALL)/scripts
	$(MKDIR) $(SERVER_INSTALL)/uploads
	$(MKDIR) $(SERVER_INSTALL)/www
	$(CP) templates/default.html $(SERVER_INSTALL)/www/index.html
	$(CP) $(BDIR)/bin/argwiad$(EXEEXT) $(SERVER_INSTALL)/bin

# Set INSTALL directories
-include mk.install

I_INC      = $(INSTALL)/include/gwiad
I_LIB      = $(INSTALL)/lib/gwiad
I_GPR      = $(INSTALL)/lib/gnat
I_INC_WEB  = $(INSTALL)/include/gwiad/web
I_INC_DL   = $(INSTALL)/include/gwiad/dl
I_INC_R    = $(INSTALL)/include/gwiad/plugins
I_INC_RS   = $(INSTALL)/include/gwiad/plugins/s
I_INC_RWS  = $(INSTALL)/include/gwiad/plugins/ws
I_BIN      = $(INSTALL)/bin

install_clean: force
ifeq ("$(INSTALL)", "")
        $(error "Wrong install path : empty INSTALL var")
endif
	echo $(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -f $(I_GPR)/gwiad.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_INC_WEB)
	$(MKDIR) $(I_INC_DL)
	$(MKDIR) $(I_INC_R)
	$(MKDIR) $(I_INC_RS)
	$(MKDIR) $(I_INC_RWS)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_GPR)

install: install_dirs
	make -C external-libs install $(OPTIONS)
	$(CP) gwiad/src/*.ad[sb] $(I_INC)
	$(CP) dynamic_libraries/src/*.ad[sb] $(I_INC_DL)
	$(CP) plugins/src/*.ad[sb] $(I_INC_R)
	$(CP) plugins/services/src/*.ad[sb] $(I_INC_RS)
	$(CP) plugins/websites/src/*.ad[sb] $(I_INC_RWS)
	$(CP) web/src/*.ad[sb] $(I_INC_WEB)
	$(CP) $(BDIR)/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/lib/* $(I_LIB)
	$(CP) $(BDIR)/dl/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/dl/lib/* $(I_LIB)
	$(CP) $(BDIR)/gwiad/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/gwiad/lib/* $(I_LIB)
	$(CP) $(BDIR)/plugins/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/plugins/lib/* $(I_LIB)
	$(CP) $(BDIR)/ps/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/ps/lib/* $(I_LIB)
	$(CP) $(BDIR)/pw/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/pw/lib/* $(I_LIB)
	$(CP) $(BDIR)/web/obj/*.ali $(I_LIB)
	$(CP) $(BDIR)/web/lib/* $(I_LIB)
	$(CP) config/projects/gwiad.gpr $(I_GPR)
	$(CP) config/projects/gwiad-shared.gpr $(I_GPR)
	$(CP) config/projects/gwiad-web.gpr $(I_GPR)
	$(CP) config/projects/gwiad-dynamic_libraries.gpr $(I_GPR)
	$(CP) config/projects/gwiad-plugins.gpr $(I_GPR)
	$(CP) config/projects/gwiad-plugins-services.gpr $(I_GPR)
	$(CP) config/projects/gwiad-plugins-websites.gpr $(I_GPR)
	$(CP) .build/static/bin/argwiadctl $(I_BIN)
ifneq ("$(MANPAGE_DIR)", "")
	-$(CP) argwiadctl/doc/argwiadctl.1 $(MANPAGE_DIR)/man1/
endif
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_BIN)
endif

install-distrib:
	$(RM) -r $(DISTRIB)
	$(MKDIR) $(DISTRIB)/data
	$(MKDIR) $(DISTRIB)/bin
	$(MKDIR) $(DISTRIB)/lib/websites
	$(MKDIR) $(DISTRIB)/lib/services
	$(MKDIR) $(DISTRIB)/config
	$(MKDIR) $(DISTRIB)/templates
	$(MKDIR) $(DISTRIB)/templates/admin
	$(MKDIR) $(DISTRIB)/scripts
	$(MKDIR) $(DISTRIB)/uploads
	$(MKDIR) $(DISTRIB)/www
	$(CP) $(BDIR)/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/dl/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/gwiad/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/plugins/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/pw/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/ps/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/web/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) external-libs/morzhol/$(BDIR)/lib/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(BDIR)/bin/argwiad$(EXEEXT) $(DISTRIB)/bin
	$(CP) .build/static/bin/argwiadctl $(DISTRIB)/bin
	$(CP) $(AWS_LIB_DIR)/*$(SOEXT) $(DISTRIB)/bin
	$(CP) $(ADA_LIB_DIR)/*$(SOEXT) $(DISTRIB)/bin
	$(CP) config/scripts/unregister $(DISTRIB)/scripts
	$(CP) templates/default.html $(DISTRIB)/www/index.html
	$(TAR_DIR) $(DISTRIB).tgz $(DISTRIB)
	$(RM) -r $(DISTRIB)

install-distrib-show-name:
	@echo $(DISTRIB)
