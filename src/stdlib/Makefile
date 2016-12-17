GD_PLOT	= ../../lib/gdchart0.94b
CC	= clang

all: ../../stdlib.o ../../runtime.o $(GD_PLOT)/plot.a

../../stdlib.o:
	$(CC) -o stdlib.o -c lib.c

../../runtime.o:
	$(CC) -o runtime.o -c runtime.c

$(GD_PLOT)/plot.a:
	cd $(GD_PLOT); make all

clean:
	rm -f ../../*.o
	cd $(GD_PLOT) ; make -f makefile clean
