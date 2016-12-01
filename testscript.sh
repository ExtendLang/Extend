if [ "$#" -eq 1 ]; then
  PRINT=$1
else
  PRINT="NOT"
fi

TESTDIR=./testcases
INPUTS=inputs
REGRESSION=inputs_regression
EXPECTED=expected
TMP_DIR=./tmp
INT_OUT=.i.out
COMP_OUT=.c.o
COMP_OUTPUT=.c.out
EXP_OUT=.exp
RES_OUT=.r.out
LLVM_F=.ll
mkdir -p $TMP_DIR
counter=0
counterc=0
counteri=0
countern=0
result=0

gcc -lm -c -o tmp/std.o src/stdlib/lib.c

for f in $(ls $TESTDIR/$REGRESSION); do
  counter=$((counter+1))
  INTERPRETER_TARGET=$TMP_DIR/$f$INT_OUT
  EXTEND_TARGET=$TMP_DIR/$f$LLVM_F
  EXTEND_FILE=$TESTDIR/$REGRESSION/$f
  COMPILED_OUTPUT=$TMP_DIR/$f$COMP_OUT
  TEXT_OUTPUT=$TMP_DIR/$f$COMP_OUTPUT
  EXPECTED_OUTPUT=$TESTDIR/$EXPECTED/$f$EXP_OUT
  RESULT_OUTPUT=$TMP_DIR/$f$RES_OUT
  ./main.byte -i $EXTEND_FILE > $INTERPRETER_TARGET 2>&1
  ./main.byte -c $EXTEND_FILE > $EXTEND_TARGET 2>&1
  llc-3.8 -filetype=obj $EXTEND_TARGET -o $COMPILED_OUTPUT
  gcc -lm -o tmp/tmp $COMPILED_OUTPUT tmp/std.o
  rm $COMPILED_OUTPUT
  ./tmp/tmp > $TEXT_OUTPUT
  diff $INTERPRETER_TARGET $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    counteri=$((counteri+1))
    echo "Interpreter: PASSED ($f)"
  else
    echo "Interpreter: FAILED ($f)"
    result=$((result+1))
    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
    fi
  fi
  diff $TEXT_OUTPUT $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    counterc=$((counterc+1))
    echo "Compiler: PASSED ($f)"
  else
    echo "Compiler: FAILED ($f)"
    result=$((result+1))
    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
    fi
  fi
done

for f in $(ls $TESTDIR/$INPUTS); do
  counter=$((counter+1))
  INTERPRETER_TARGET=$TMP_DIR/$f$INT_OUT
  EXTEND_TARGET=$TMP_DIR/$f$LLVM_F
  EXTEND_FILE=$TESTDIR/$INPUTS/$f
  COMPILED_OUTPUT=$TMP_DIR/$f$COMP_OUT
  EXPECTED_OUTPUT=$TESTDIR/$EXPECTED/$f$EXP_OUT
  TEXT_OUTPUT=$TMP_DIR/$f$COMP_OUTPUT
  RESULT_OUTPUT=$TMP_DIR/$f$RES_OUT
  p=0
  ./main.byte -i $EXTEND_FILE > $INTERPRETER_TARGET 2>&1
  ./main.byte -c $EXTEND_FILE > $EXTEND_TARGET 2>&1
  llc-3.8 -filetype=obj $EXTEND_TARGET -o $COMPILED_OUTPUT
  gcc -o tmp/tmp $COMPILED_OUTPUT tmp/std.o
  rm $COMPILED_OUTPUT
  ./tmp/tmp > $TEXT_OUTPUT
  diff $INTERPRETER_TARGET $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    counteri=$((counteri+1))
#    p=$((p+1))
    echo "Interpreter: PASSED ($f)"
  else
    echo "Interpreter: FAILED ($f)"
    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
    fi
  fi
  diff $TEXT_OUTPUT $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    counterc=$((counterc+1))
    p=$((p+1))
    echo "Compiler: PASSED ($f)"
  else
    echo "Compiler: FAILED ($f)"
    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
    fi
  fi
  if [ $p -eq 1 ]; then
    countern=$((countern+1))
    mv $EXTEND_FILE $TESTDIR/$REGRESSION/$f
  fi
done

rm tmp/std.o

echo "Passed $counteri of $counter interpreter testcases"
echo "Passed $counterc of $counter compiler testcases"
echo "$countern new testcases passed, $result regression tests failed"
exit $result
