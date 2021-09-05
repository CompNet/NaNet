all: zeta_func.o
	gcc -LD:\MinGW\GSL\lib zeta_func.o -lgsl -lgslcblas -lm -o zeta_func

zeta_func.o:
	gcc -ID:\MinGW\GSL\include -c zeta_func.c

