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

all: check_mem

force:
setup:

clean:
	-$(GNATCLEAN) -q -Pregtests
	rm -f alldifs run1 run2 check_mem.run1 check_mem.run2
	rm -f libgwiad_admin$(EXEEXT)
	rm -f libhello_world_service$(EXEEXT)
	rm -f libhello_world_website$(EXEEXT)

check_mem: force
	$(GNATMAKE) -Pregtests -XPRJ_BUILD="$(MODE)"
	make -C ../admin/
	make -C ../example/ build
	cp ../$(BDIR)/slib/websites/lib* .
	cp ../$(BDIR)/slib/services/lib* .
	echo -n "Running " ../$(BDIR)/bin/check_mem$(EXEEXT)
	../$(BDIR)/bin/check_mem$(EXEEXT) 2 > check_mem.res
	gnatmem 3 -i gmem.out ../$(BDIR)/bin/check_mem$(EXEEXT) > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	../$(BDIR)/bin/check_mem$(EXEEXT) 30 >> check_mem.res
	gnatmem 3 -i gmem.out ../$(BDIR)/bin/check_mem$(EXEEXT) > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	$(DIFF) run1 run2 1>> alldifs 2>> alldifs
