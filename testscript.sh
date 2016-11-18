if [ "$#" -eq 1 ]; then
  PRINT=$1
else
  PRINT="NOT"
fi

TESTDIR=./testcases
INPUTS=inputs
EXPECTED=expected
TMP_DIR=./tmp
INT_OUT=.i.out
COMP_OUT=.c.out
EXP_OUT=.exp
RES_OUT=.r.out
LLVM_F=.ll
mkdir -p $TMP_DIR
for f in $(ls $TESTDIR/$INPUTS); do
  INTERPRETER_TARGET=$TMP_DIR/$f$INT_OUT
  EXTEND_TARGET=$TMP_DIR/$f$LLVM_F
  EXTEND_FILE=$TESTDIR/$INPUTS/$f
  COMPILED_OUTPUT=$TMP_DIR/$f$COMP_OUT
  EXPECTED_OUTPUT=$TESTDIR/$EXPECTED/$f$EXP_OUT
  RESULT_OUTPUT=$TMP_DIR/$f$RES_OUT
  ./main.byte -i $EXTEND_FILE > $INTERPRETER_TARGET 2>&1
  ./main.byte -c $EXTEND_FILE > $EXTEND_TARGET 2>&1
  lli $EXTEND_TARGET arg1 > $COMPILED_OUTPUT 2>&1
  diff $INTERPRETER_TARGET $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    echo "Interpreter: PASSED ($f)"
  else
    echo "Interpreter: FAILED ($f)"
    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
    fi
  fi
  diff $COMPILED_OUTPUT $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    echo "Compiler: PASSED ($f)"
  else
    echo "Compiler: FAILED ($f)"
    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
    fi
  fi
done
