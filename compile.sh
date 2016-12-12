LIB=stdlib.o
RUNTIME=runtime.op
gcc -c src/stdlib/lib.c -o $LIB
gcc -c src/stdlib/runtime.c -o $RUNTIME
./main.byte -c $1 -l
rm $LIB
