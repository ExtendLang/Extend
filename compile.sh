LIB=stdlib.o
RUNTIME=runtime.o
COMPILER=clang
$COMPILER -c src/stdlib/lib.c -o $LIB
$COMPILER -c src/stdlib/runtime.c -o $RUNTIME
./main.byte -cc $COMPILER -c $1 -l
rm $LIB
rm $RUNTIME
