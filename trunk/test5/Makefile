all: test5

test5: test5.o ../lib/libiphreeqc.a
	g++ -L../lib -o test5 test5.o -liphreeqc

test5.o: test5.c
	gcc -I../include -c test5.c

clean:
	$(RM) test5 test5.o *~