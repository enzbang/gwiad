GNATMAKE = gnatmake
GNATCLEAN = gnatclean
DIFF = diff
EXEEXT =

all: check_mem

force:

check_mem: force
	$(GNATMAKE) -Pregtests
	echo -n "Running " check_mem
	./check_mem 2 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem 30 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	$(GNATCLEAN) -q -Pregtests
	-$(DIFF) run1 run2 1>> alldifs 2>> alldifs
