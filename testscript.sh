TESTDIR=./testcases
TMP_DIR=./tmp
INT_OUT=.i.out
COMP_OUT=.c.out
RES_OUT=.r.out
LLVM_F=.ll
BIN_F=$COMP_F.o
mkdir -p $TMP_DIR
for f in $(ls $TESTDIR); do
  INTERPRETER_TARGET=$TMP_DIR/$f$INT_OUT
  EXTEND_TARGET=$TMP_DIR/$f$LLVM_F
  EXTEND_FILE=$TESTDIR/$f
  COMPILED_OUTPUT=$TMP_DIR/$f$COMP_OUT
  RESULT_OUTPUT=$TMP_DIR/$f$RES_OUT
  ./main.byte -i $EXTEND_FILE &> $INTERPRETER_TARGET
  ./main.byte -c $EXTEND_FILE &> $EXTEND_TARGET
  lli-3.4 $EXTEND_TARGET arg1 &> $COMPILED_OUTPUT
  diff $INTERPRETER_TARGET $COMPILED_OUTPUT &> $RESULT_OUTPUT
  if [ $? -eq 0 ]; then
      echo "PASSED ($f)"
  else
      echo "FAILED ($f)"
      cat $RESULT_OUTPUT
  fi
done
