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
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -XLIB_TYPE=Static -Pargwiadctl
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -XLIB_TYPE=Static -Pargwiadctl

build_doc:
	a2x -f manpage doc/argwiadctl.txt 2>/dev/null

clean:
	-$(GNATCLEAN) -r -Pargwiadctl

check:
	$(GNATCHECK) -dd -Pargwiadctl -rules -from=../gwiad.check

setup:
