all: exp_int.o
	gcc -LD:\MinGW\GSL\lib exp_int.o -lgsl -lgslcblas -lm -o exp_int

exp_int.o:
	gcc -ID:\MinGW\GSL\include -c exp_int.c

