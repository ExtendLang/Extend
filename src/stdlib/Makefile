GD_PLOT	= ../../lib/gdchart0.94b
CC	= clang

all: stdlib.o runtime.o ../../stdlib.a

../../stdlib.a: runtime.o stdlib.o
	cd $(GD_PLOT) ; make all
	ar rc ../../stdlib.a *.o $(GD_PLOT)/*.o $(GD_PLOT)/gd1.3/*.o

stdlib.o:
	$(CC) -o stdlib.o -c lib.c

runtime.o:
	$(CC) -o runtime.o -c runtime.c

clean:
	rm -f ../../*.o
	rm -f *.o
	rm -f ../../stdlib.a
	make -C $(GD_PLOT) -f makefile clean
