#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<stdbool.h>

/* Value type */
#define FLAG_EMPTY 0
#define FLAG_NUMBER 1
#define FLAG_STRING 2
#define FLAG_SUBRANGE 3

/* Status flag */
#define CALCULATED 2
#define IN_PROGRESS 4

struct subrange_t;
struct value_t;

struct formula_t {
	struct value_t (*formula)(struct value_t);
};

typedef struct formula_t* formula_p;

struct string_t {
	char *text;
	long length;
	int refs;
};

typedef struct string_t* string_p;

struct value_t {
	char flags;
	double numericVal;
	string_p str;
	struct subrange_t *subrange;
};

typedef struct value_t* value_p;

struct status_t {
	int calcStatus;
};

typedef struct status_t* status_p;

struct range_t {
	int rows;
	int cols;
	value_p values;
	status_p statuses;
	formula_p formulas;
};

typedef struct range_t* range_p;

struct subrange_t {
	range_p range;
	int offsetRow;
	int offsetCol;
	int subrangeRow;
	int subrangeCol;
};

typedef struct subrange_t* subrange_p;

//value_p get_val(range_p range, int row, int col);

value_p get_val(subrange_p range, int row, int col) {
	//TODO: assertions
	value_p val = range->range->values + (row + range->offsetRow) * range->range->cols + col + range->offsetCol;
	return val;
}

double setNumeric(value_p result, double val) {
	return (result->numericVal = val);
}

double setFlag(value_p result, double flag_num) {
	return (result->flags = FLAG_NUMBER);
}

int assertSingle(subrange_p range) {
	return (range->subrangeRow == 1 && range->subrangeCol == 1);
}

int assertText(value_p my_val) {
	return (my_val->flags == FLAG_STRING);
}

value_p new_val() {
	value_p empty_val = malloc(sizeof(struct value_t));
	setFlag(empty_val, FLAG_EMPTY);
	return empty_val;
}

value_p print(subrange_p whatever, subrange_p text) {
	if(!assertSingle(text)) return new_val();
	value_p my_val = get_val(text,0,0);
	if(!assertText(my_val)) return new_val();
	printf("%s", my_val->str->text);
	return new_val();
}

value_p printd(subrange_p whatever, subrange_p text) {
	printf("%f\n", text->range->values->numericVal);
	value_p result = malloc(sizeof(struct value_t));
	return result;
}

value_p extend_sin(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = sin(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_cos(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = cos(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_tan(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = tan(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_asin(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = asin(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_acos(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = acos(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_atan(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = atan(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_sinh(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = sinh(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_cosh(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = cosh(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_tanh(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = tanh(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_exp(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = exp(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_log(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = log(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_log10(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = log10(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_sqrt(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = sqrt(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_ceil(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = ceil(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_fabs(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = fabs(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

value_p extend_floor(subrange_p range) {
	double val;
	if(!assertSingle(range)) return new_val();
	value_p initial = get_val(range, 0, 0);
	val = floor(initial->numericVal);
	value_p result = new_val();
	setNumeric(result,val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

/*
 * VENDOR
 */
struct ExtendScope;
typedef value_p (*FormulaFP) (struct ExtendScope *scope, int row, int col);

struct ExtendFormula {
  /* These 10 variables correspond to formula_row_start through formula_col_end,
   * where bool singleRow/Col are true if formula_row_end is None */
  bool fromFirstRow;
  int rowStart_varnum;
  bool toLastRow;
  int rowEnd_varnum;
  bool fromFirstCol;
  int colStart_varnum;
  bool toLastCol;
  int colEnd_varnum;

  FormulaFP formula;
};
struct var_defn {
  /* This is like a class definition - for every declared variable in the
   * Extend source, there should be one instance of these per compiled program.
   * They should just live in the global program storage.
   * It corresponds to Ast.variable */

   int rows_varnum;
   int cols_varnum;
   int numFormulas;
   struct ExtendFormula *formulas;
};
struct var_instance {
  /* This is an actual instance of a variable - we get one of these
   * per variable per time a function is called (assuming the contents
   * of the variable get examined.  */
  struct var_defn defi;
  struct ExtendScope *closure;
  char *status;
  struct value_t *values;
};
struct ExtendScope {
  struct var_defn *defns;
  struct var_instance *vars;
	int numVars;
};
struct ExtendScope *global_scope;

bool assertInBounds(struct var_defn *defn, int x, int y) {
	if(defn->rows_varnum < x && defn->cols_varnum < y) return true;
	return false;
}

bool fitsDim(int dim, bool fromFirstRow, int rowStart_varnum, bool toLastRow, int rowEnd_varnum) {
	return (fromFirstRow || (dim >= rowStart_varnum)) && (toLastRow || (dim <= rowEnd_varnum));
}

bool fitsRange(struct ExtendFormula *formula, int x, int y) {
	return fitsDim(x, formula->fromFirstRow, formula->rowStart_varnum, formula->toLastRow, formula->rowEnd_varnum)
		&& fitsDim(y, formula->fromFirstCol, formula->colStart_varnum, formula->toLastCol, formula->colEnd_varnum);
}

int calcVal(struct var_instance *inst, int x, int y, value_p target) {
	struct ExtendFormula *form = inst->defi.formulas;
	while(form < inst->defi.formulas + inst->defi.numFormulas) {
		if(fitsRange(form, x, y)) {
			goto found;
		}
		form++;
	}
	return -1;
found: {
		value_p res = (form->formula)(inst->closure, x, y);
		memcpy(target, res, sizeof(struct value_t));
		return 0;
	}
}

value_p getVal(struct var_instance *inst, int x, int y) {
	if(!assertInBounds(&inst->defi, x, y)) return new_val();
	int offset = inst->defi.rows_varnum * y + x;
	char *status = inst->status + offset;
	if(*status & IN_PROGRESS) {
		/* TODO: Circular dependency. Possibly throw? */
		return new_val();
	} else if (!(*status) & CALCULATED) { /* value not calculated */
		calcVal(inst, x, y, inst->values + offset);
	}
	return (inst->values + offset);
}
