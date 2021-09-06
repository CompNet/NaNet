all: zeta_func.o
	gcc -LC:\MinGW\GSL\lib zeta_func.o -lgsl -lgslcblas -lm -o zeta_func

zeta_func.o:
	gcc -IC:\MinGW\GSL\include -c zeta_func.c

