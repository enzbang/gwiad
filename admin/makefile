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

build:
ifeq ($(shell ls tsrc/ | wc -l), 0)
	$(error "Please run 'make setup'")
endif
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -Pgwiad-admin
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -Pgwiad-admin

setup:
	-$(MKDIR) gen tsrc
	templates2ada -d templates/ -o gen/templates.cds \
		-t templates/templates.tads
	$(GNATCHOP) -wpq gen/templates.cds tsrc/
	$(RM) gen/templates.cds
	$(RM) -r gen

clean:
	-$(GNATCLEAN) -r -Pgwiad-admin
	$(RM) -r tsrc

check:
	$(GNATCHECK) -dd -Pgwiad-admin -rules -from=../gwiad.check