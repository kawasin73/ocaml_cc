#
# test.mk
#

ns := $(shell seq -f "%02.0f" 0 74)
gcc_exes := $(patsubst %,exe/f%.gcc,$(ns))
occ_exes := $(patsubst %,exe/f%.occ,$(ns))
checks := $(patsubst %,check/f%.check,$(ns))

all : fun.s $(gcc_exes) $(occ_exes) $(checks)

exe/created :
	mkdir -p exe/created

check/created :
	mkdir -p check/created

$(gcc_exes) : exe/f%.gcc : ../test/fun.c ../test/main.c exe/created
	gcc -o $@ -Dfoo=f$* ../test/main.c ../test/fun.c -O0 -g

$(occ_exes) : exe/f%.occ : fun_occ.s ../test/main.c exe/created
	gcc -o $@ -Dfoo=f$* ../test/main.c fun_occ.s -O0 -g

$(checks) : check/f%.check : exe/f%.gcc exe/f%.occ check/created
	./cmp_gcc_occ.py exe/f$* 10 -5 5           > $@
	./cmp_gcc_occ.py exe/f$* 10 -1000 1000    >> $@
	./cmp_gcc_occ.py exe/f$* 10 -10000 10000  >> $@
	cat $@

fun.s : ../test/fun.c
	gcc -O3 -S $< -o $@

fun.i : ../test/fun.c
	gcc -P -E $< -o $@

fun_occ.s : fun.i ./cc.byte
	./cc.byte $< $@

clean :
	rm -rf fun_occ.s fun.i fun.s check exe

.DELETE_ON_ERROR:
