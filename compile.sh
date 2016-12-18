LIB=stdlib.o
RUNTIME=runtime.o
COMPILER=clang
make -C src/stdlib PLOT=1
./main.byte -cc $COMPILER -c $1 -l
make -C src/stdlib clean
