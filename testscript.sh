if [ "$#" -eq 1 ]; then
  PRINT=$1
else
  PRINT="NOT"
fi

COMPILER=clang
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
rm ./$TMP_DIR/*
counter=0
counterc=0
counteri=0
countern=0
result=0

$COMPILER -c -o stdlib.o src/stdlib/lib.c
if [ $? -ne 0 ]; then
	rm stdlib.o;
	exit -1;
fi
$COMPILER -c -o runtime.o src/stdlib/runtime.c
if [ $? -ne 0 ]; then
	rm stdlib.o runtime.o
	exit -1;
fi

for f in $(ls $TESTDIR/$REGRESSION); do
  counter=$((counter+1))
  EXTEND_TARGET=$TMP_DIR/$f$LLVM_F
  EXTEND_FILE=$TESTDIR/$REGRESSION/$f
  COMPILED_OUTPUT=$TMP_DIR/$f$COMP_OUT
  TEXT_OUTPUT=$TMP_DIR/$f$COMP_OUTPUT
  EXPECTED_OUTPUT=$TESTDIR/$EXPECTED/$f$EXP_OUT
  RESULT_OUTPUT=$TMP_DIR/$f$RES_OUT
  ./main.byte -cc $COMPILER -c $EXTEND_FILE -l > $EXTEND_TARGET 2>&1
  if [ $? -eq 0 ]; then
    ./out > $TEXT_OUTPUT
  else
    mv $EXTEND_TARGET $TEXT_OUTPUT
  fi
  diff $TEXT_OUTPUT $EXPECTED_OUTPUT > $RESULT_OUTPUT 2>&1
  if [ $? -eq 0 ]; then
    counterc=$((counterc+1))
    echo "Compiler: PASSED ($f)"
  else
    echo "Compiler: FAILED REGRESSION TEST ($f)"
    result=$((result+1))
#    if [ $PRINT = "-p" ]; then
      cat $RESULT_OUTPUT
#    fi
  fi
done

for f in $(ls $TESTDIR/$INPUTS); do
  counter=$((counter+1))
  EXTEND_TARGET=$TMP_DIR/$f$LLVM_F
  EXTEND_FILE=$TESTDIR/$INPUTS/$f
  COMPILED_OUTPUT=$TMP_DIR/$f$COMP_OUT
  EXPECTED_OUTPUT=$TESTDIR/$EXPECTED/$f$EXP_OUT
  TEXT_OUTPUT=$TMP_DIR/$f$COMP_OUTPUT
  RESULT_OUTPUT=$TMP_DIR/$f$RES_OUT
  p=0
  ./main.byte -cc $COMPILER -c $EXTEND_FILE -l > $EXTEND_TARGET 2>&1
  if [ $? -eq 0 ]; then
    ./out > $TEXT_OUTPUT
  else
    mv $EXTEND_TARGET $TEXT_OUTPUT
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

rm stdlib.o
rm runtime.o

echo "Passed $counterc of $counter compiler testcases"
echo "$countern new testcases passed, $result regression tests failed"
exit $result
