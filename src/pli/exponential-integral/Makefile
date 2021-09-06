all: exp_int.o
	gcc -LC:\MinGW\GSL\lib exp_int.o -lgsl -lgslcblas -lm -o exp_int

exp_int.o:
	gcc -IC:\MinGW\GSL\include -c exp_int.c

