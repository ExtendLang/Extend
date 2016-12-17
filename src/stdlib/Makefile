GD_PLOT	= ../../lib/gdchart0.94b
CC	= clang

all: ../../stdlib.o ../../runtime.o $(GD_PLOT)/plot.a ../../stdlib.a

../../stdlib.a: ../../stdlib.o $(GD_PLOT)/plot.a
	ar rc ../../stdlib.a ../../stdlib.o $(GD_PLOT)/price_conv.o $(GD_PLOT)/gdc.o $(GD_PLOT)/gdc_pie.o $(GD_PLOT)/gdchart.o 

../../stdlib.o:
	$(CC) -o ../../stdlib.o -c lib.c

../../runtime.o:
	$(CC) -o ../../runtime.o -c runtime.c

$(GD_PLOT)/plot.a:
	cd $(GD_PLOT); make all

clean:
	rm -f ../../*.o
	cd $(GD_PLOT) ; make -f makefile clean
