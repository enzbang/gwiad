##############################################################################
##                                  Gwiad                                   ##
##                                                                          ##
##                           Copyright (C) 2007                             ##
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

GENERAL_OPTIONS = CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" \
	GNATMAKE="$(GNATMAKE)" GNATCLEAN="$(GNATCLEAN)" EXEEXT="$(EXEEXT)" \
	DIFF="$(DIFF)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)

# Modules support

MODULES =  gwiad dynamic_libraries plugins web admin example argwiad

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_CLEAN = ${MODULES:%=%_clean}

MODULES_CHECK = ${MODULES:%=%_check}

MODULES_SETUP = ${MODULES:%=%_setup}

ifeq ("$(INSTALL)", "..")
$(error "Wrong install path : INSTALL='$(INSTALL)'")
else
ifeq ("$(INSTALL)", "")
$(error "Wrong install path : empty INSTALL var")
endif
endif

# Targets

all: $(MODULES_BUILD)

build: all

clean: $(MODULES_CLEAN)
	make -C regtests clean $(OPTIONS)

setup: $(MODULES_SETUP)
	$(MKDIR) lib/websites lib/services

regtests: force
	make -C regtests MODE="Profile" $(GENERAL_OPTIONS)
	rm -f regtests/obj/*	# To avoid error in lcov_analyse ???
	make lcov_analyse

lcov_analyse: force
	sh analyse.sh

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build_doc $(GALL_OPTIONS)

check :	check_message $(MODULES_CHECK)
	@echo
	@echo "#####################################"
	@echo "### Check style with style_checker ##"
	@echo "#####################################"
	@echo
	-find . -not -name "b~*" -a \( -name "*.adb" -o -name "*.ads" \) \
	-and -not -path "*tsrc*" | xargs style_checker -lang Ada -cp -cy -sp -gnat05

check_message:
	@echo
	@echo "############################################"
	@echo "### Verifying Properties Using gnatcheck ###"
	@echo "############################################"
	@echo

force:

# Install directories

I_BIN	   = $(INSTALL)/bin
I_MORZ     = $(INSTALL)/include/morzhol
I_INC	   = $(INSTALL)/include/gwiad
I_INC_WEB  = $(INSTALL)/include/gwiad/web
I_INC_DL   = $(INSTALL)/include/gwiad/dl
I_INC_R    = $(INSTALL)/include/gwiad/plugins
I_INC_RS   = $(INSTALL)/include/gwiad/plugins/s
I_INC_RWS  = $(INSTALL)/include/gwiad/plugins/ws
I_LIB_MORZ = $(INSTALL)/lib/morzhol
I_LIB	   = $(INSTALL)/lib/gwiad
I_GPR	   = $(INSTALL)/lib/gnat

DEMO_DISTRIB = demo_distrib

${MODULES_BUILD}:
	${MAKE} -C ${@:%_build=%} $(OPTIONS)

${MODULES_CLEAN}:
	${MAKE} -C ${@:%_clean=%} clean $(OPTIONS)

${MODULES_CHECK}:
	${MAKE} -C ${@:%_check=%} check $(OPTIONS)

${MODULES_SETUP}:
	${MAKE} -C ${@:%_setup=%} setup $(OPTIONS)


install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -f $(I_GPR)/gwiad.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_MORZ)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_INC_WEB)
	$(MKDIR) $(I_INC_DL)
	$(MKDIR) $(I_INC_R)
	$(MKDIR) $(I_INC_RS)
	$(MKDIR) $(I_INC_RWS)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_LIB_MORZ)
	$(MKDIR) $(I_GPR)

install: install_dirs
	$(CP) external-libs/morzhol/src/*.ad[sb] $(I_MORZ)
	$(CP) gwiad/src/*.ad[sb] $(I_INC)
	$(CP) dynamic_libraries/src/*.ad[sb] $(I_INC_DL)
	$(CP) plugins/src/*.ad[sb] $(I_INC_R)
	$(CP) plugins/services/src/*.ad[sb] $(I_INC_RS)
	$(CP) plugins/websites/src/*.ad[sb] $(I_INC_RWS)
	$(CP) web/src/*.ad[sb] $(I_INC_WEB)
	$(CP) external-libs/morzhol/lib/* $(I_LIB_MORZ)
	$(CP) plugins/obj/*.ali $(I_LIB)
	$(CP) gwiad/lib/gwiad.ali $(I_INC)
	$(CP) gwiad/lib/* $(I_LIB)
	$(CP) dynamic_libraries/lib/* $(I_LIB)
	$(CP) plugins/lib/* $(I_LIB)
	$(CP) plugins/websites/lib/* $(I_LIB)
	$(CP) plugins/services/lib/* $(I_LIB)
	$(CP) web/lib/* $(I_LIB)
	$(CP) config/projects/morzhol.gpr $(I_GPR)
	$(CP) config/projects/gwiad.gpr $(I_GPR)
	$(CP) config/projects/gwiad-shared.gpr $(I_GPR)
	$(CP) config/projects/gwiad-web.gpr $(I_GPR)
	$(CP) config/projects/gwiad-dynamic_libraries.gpr $(I_GPR)
	$(CP) config/projects/gwiad-plugins.gpr $(I_GPR)
	$(CP) config/projects/gwiad-plugins-services.gpr $(I_GPR)
	$(CP) config/projects/gwiad-plugins-websites.gpr $(I_GPR)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/..
	$(CP) $(I_LIB_MORZ)/*$(SOEXT) $(I_LIB)/..
endif

install_demo: install_server
	$(CP) -r lib/services/libhello_world_service.$(SOEXT) \
		$(SERVER_INSTALL)/lib/services/
	$(CP) -r lib/websites/libhello_world_website.$(SOEXT) \
		$(SERVER_INSTALL)/lib/websites/
	$(CP) example/hello_world_interface/lib/*$(SOEXT) $(SERVER_INSTALL)/bin

install_admin_plugin:
	$(CP) -r admin/templates/*.thtml \
		$(SERVER_INSTALL)/templates/admin
	$(CP) -r lib/websites/libgwiad_admin.$(SOEXT) \
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
	$(CP) argwiad/bin/argwiad$(EXEEXT) $(SERVER_INSTALL)/bin

demo_distrib:
	$(RM) -r $(DEMO_DISTRIB)
	$(MKDIR) $(DEMO_DISTRIB)/data
	$(MKDIR) $(DEMO_DISTRIB)/bin
	$(MKDIR) $(DEMO_DISTRIB)/lib/websites
	$(MKDIR) $(DEMO_DISTRIB)/lib/services
	$(MKDIR) $(DEMO_DISTRIB)/config
	$(MKDIR) $(DEMO_DISTRIB)/templates
	$(MKDIR) $(DEMO_DISTRIB)/templates/services_admin
	$(MKDIR) $(DEMO_DISTRIB)/templates/websites_admin
	$(MKDIR) $(DEMO_DISTRIB)/scripts
	$(MKDIR) $(DEMO_DISTRIB)/librairies
## Copy all shared libraries found in argwiad to $(DEMO_DISTRIB)/bin
ifneq ($(OS),Windows_NT)
	-sh config/scripts/ldd_copy example/demo/bin/argwiad \
		$(DEMO_DISTRIB)/bin
endif
	$(CP) -r lib/services/*$(SOEXT) $(DEMO_DISTRIB)/lib/services
	$(CP) -r lib/websites/*$(SOEXT) $(DEMO_DISTRIB)/lib/websites
	$(CP) config/scripts/unregister $(DEMO_DISTRIB)/scripts
	$(CP) templates/*html $(DEMO_DISTRIB)/templates/
	$(CP) -r templates/websites_admin/*.thtml \
		$(DEMO_DISTRIB)/templates/websites_admin
	$(CP) -r templates/services_admin/*.thtml \
		$(DEMO_DISTRIB)/templates/services_admin
	$(CP) example/hello_world_interface/lib/*$(SOEXT) $(DEMO_DISTRIB)/bin
	$(CP) example/demo/bin/argwiad$(EXEEXT) $(DEMO_DISTRIB)/bin
	$(CP) example/demo/start_demo.sh $(DEMO_DISTRIB)/
	$(TAR_DIR) $(DEMO_DISTRIB).tgz $(DEMO_DISTRIB)
	$(RM) -r $(DEMO_DISTRIB)
