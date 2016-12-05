TMP_DIR=tmp
LIB=lib.o
./main.byte -c $1 > $TMP_DIR/inter.ll
gcc -c src/stdlib/lib.c -o $TMP_DIR/$LIB
gcc $TMP_DIR/inter.ll $TMP_DIR/$LIB -o out
rm $TMP_DIR/$LIB
