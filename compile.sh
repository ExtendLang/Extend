LIB=stdlib.o
gcc -c src/stdlib/lib.c -o $LIB
./main.byte -c $1 -l
rm $LIB
