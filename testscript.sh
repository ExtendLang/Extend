TESTDIR=./testcases
TMP_DIR=./tmp
INT_OUT=interpreter.out
COMP_OUT=compiled.out
RES_OUT=res.out
COMP_F=comp
LLVM_F=$COMP_F.ll
BIN_F=$COMP_F.o
make >/dev/null
mkdir -p $TMP_DIR
for f in $(ls $TESTDIR); do
  ./main.byte -i $TESTDIR/$f &> $TMP_DIR/$INT_OUT
  ./main.byte -c $TESTDIR/$f &> $TMP_DIR/$LLVM_F
  lli-3.4 $TMP_DIR/$LLVM_F &> $TMP_DIR/$COMP_OUT
  diff $TMP_DIR/$INT_OUT $TMP_DIR/$COMP_OUT &> $TMP_DIR/$RES_OUT
  if [ $? -eq 0 ]; then
      echo "PASSED ($f)"
  else
      echo "FAILED ($f)"
  fi
done
