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

OPTIONS = MODE="$(MODE)" CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)"

ifeq ($(OS),Windows_NT)
SOEXT=.dll
EXEEXT=.exe
else
SOEXT=.so
EXEEXT=
endif

# Modules support

MODULES =  gwiad services dynamic_libraries web admin example

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_CLEAN = ${MODULES:%=%_clean}

MODULES_CHECK = ${MODULES:%=%_check}

# Targets

all: $(MODULES_BUILD)

clean: $(MODULES_CLEAN)

check :$(MODULES_CHECK)

# Install directories

I_BIN	   = $(INSTALL)/bin
I_INC	   = $(INSTALL)/include/gwiad
I_INC_WEB  = $(INSTALL)/include/gwiad/web
I_INC_DL   = $(INSTALL)/include/gwiad/dl
I_INC_S    = $(INSTALL)/include/gwiad/s
I_INC_WS   = $(INSTALL)/include/gwiad/ws
I_INC_ECWF = $(INSTALL)/include/gwiad/ecwf
I_LIB	   = $(INSTALL)/lib/gwiad
I_GPR	   = $(INSTALL)/lib/gnat

DEMO_DISTRIB = demo_distrib

${MODULES_BUILD}:
	${MAKE} -C ${@:%_build=%} $(OPTIONS)

${MODULES_CLEAN}:
	${MAKE} -C ${@:%_clean=%} clean $(OPTIONS)

${MODULES_CHECK}:
	${MAKE} -C ${@:%_check=%} check $(OPTIONS)

install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -f $(I_GPR)/gwiad.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_INC_ECWF)
	$(MKDIR) $(I_INC_WEB)
	$(MKDIR) $(I_INC_DL)
	$(MKDIR) $(I_INC_S)
	$(MKDIR) $(I_INC_WS)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_GPR)

install: install_dirs
	$(CP) gwiad/src/*.ad[sb] $(I_INC)
	$(CP) gwiad/ecwf/*.ad[sb] $(I_INC_ECWF)
	$(CP) dynamic_libraries/src/*.ad[sb] $(I_INC_DL)
	$(CP) services/src/*.ad[sb] $(I_INC_S)
	$(CP) websites/src/*.ad[sb] $(I_INC_WS)
	$(CP) web/src/*.ad[sb] $(I_INC_WEB)
	$(CP) gwiad/lib/gwiad.ali $(I_INC)
	$(CP) gwiad/lib/* $(I_LIB)
	$(CP) dynamic_libraries/lib/* $(I_LIB)
	$(CP) services/lib/* $(I_LIB)
	$(CP) websites/lib/* $(I_LIB)
	$(CP) web/lib/* $(I_LIB)
	$(CP) config/projects/gwiad.gpr $(I_GPR)
	$(CP) config/projects/gwiad-ecwf.gpr $(I_GPR)
	$(CP) config/projects/gwiad-web.gpr $(I_GPR)
	$(CP) config/projects/gwiad-dynamic_libraries.gpr $(I_GPR)
	$(CP) config/projects/gwiad-services.gpr $(I_GPR)
	$(CP) config/projects/gwiad-websites.gpr $(I_GPR)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/..
endif

install_demo:
	$(MKDIR) $(DEMO_INSTALL)/data
	$(MKDIR) $(DEMO_INSTALL)/bin
	$(MKDIR) $(DEMO_INSTALL)/lib/websites
	$(MKDIR) $(DEMO_INSTALL)/lib/services
	$(MKDIR) $(DEMO_INSTALL)/templates
	$(MKDIR) $(DEMO_INSTALL)/templates/services_admin
	$(MKDIR) $(DEMO_INSTALL)/templates/websites_admin
	$(MKDIR) $(DEMO_INSTALL)/scripts
	$(CP) -r lib/services/*$(SOEXT) $(DEMO_INSTALL)/lib/services/
	$(CP) -r lib/websites/*$(SOEXT) $(DEMO_INSTALL)/lib/websites/
	$(CP) templates/*html $(DEMO_INSTALL)/templates/
	$(CP) config/scripts/unregister $(DEMO_INSTALL)/scripts
	$(CP) -r templates/websites_admin/*.thtml \
		$(DEMO_INSTALL)/templates/websites_admin
	$(CP) -r templates/services_admin/*.thtml \
		$(DEMO_INSTALL)/templates/services_admin
	$(CP) example/hello_world_interface/lib/*$(SOEXT) $(DEMO_INSTALL)/bin
	$(CP) example/demo/bin/argwiad$(EXEEXT) $(DEMO_INSTALL)/bin

demo_distrib:
	$(RM) -r $(DEMO_DISTRIB)
	$(MKDIR) $(DEMO_DISTRIB)/data
	$(MKDIR) $(DEMO_DISTRIB)/bin
	$(MKDIR) $(DEMO_DISTRIB)/lib/websites
	$(MKDIR) $(DEMO_DISTRIB)/lib/services
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
