GD_PLOT	= ../../lib/gdchart0.94b
CC	= clang

all: ../../stdlib.o ../../runtime.o

../../stdlib.o:
	$(CC) -o stdlib.o -c lib.c

../../runtime.o:
	$(CC) -o runtime.o -c runtime.c

$(GD_LIB)/plot.a:
	cd $(GD_LIB); make

clean:
	rm -f ../../*.o
	cd $(GD_PLOT) ; make -f makefile clean
