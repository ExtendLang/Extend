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

#define MAX_FILES 255
FILE *open_files[1 + MAX_FILES] = {NULL};
int open_num_files = 0;

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

struct ResolvedFormula {
	int rowStart, rowEnd, colStart, colEnd;
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
	 bool isOneByOne;
};
struct var_instance {
  /* This is an actual instance of a variable - we get one of these
   * per variable per time a function is called (assuming the contents
   * of the variable get examined.  */
	int rows, cols;
	int numFormulas;
	struct ResolvedFormula *formulas;
  struct ExtendScope *closure;
  value_p *values;
	char *status;
};
struct ExtendScope {
  struct var_defn *defns;
  struct var_instance **vars;
	int numVars;
};

struct subrange_t {
	struct var_instance *range;
	int offsetRow;
	int offsetCol;
	int subrangeRow;
	int subrangeCol;
};

typedef struct subrange_t* subrange_p;

string_p new_string(char *str);

value_p box_value_string(string_p);

value_p __get_val(struct var_instance *range, int row, int col) {
	//TODO: assertions
	printf("Getting %p %d %d\n", range, row, col);
	value_p val = range->values[row * range->cols + col];
	return val;
}

value_p get_val(subrange_p range, int row, int col) {
	//TODO: assertions
	value_p val =  __get_val(range->range, row + range->offsetRow, col + range->offsetCol);
	return val;
}

double setNumeric(value_p result, double val) {
	return (result->numericVal = val);
}

char* setString(value_p result, char *str) {
	return (result->str->text = str);
}

double setFlag(value_p result, double flag_num) {
	return (result->flags = FLAG_NUMBER);
}

int assertSingle(subrange_p range) {
	return (range->subrangeRow == 1 && range->subrangeCol == 1);
}

int assertSingleNumber(subrange_p range) {
	if (!assertSingle(range)) {
		return 0;
	};
	value_p p = get_val(range, 0, 0);
	return (p->flags == FLAG_NUMBER);
}

int assertText(value_p my_val) {
	return (my_val->flags == FLAG_STRING);
}

int assertSingleString(subrange_p range) {
	if (!assertSingle(range)) {
		return 0;
	}
	value_p p = get_val(range, 0, 0);
	return (p->flags == FLAG_STRING);
}

value_p new_val() {
	value_p empty_val = malloc(sizeof(struct value_t));
	setFlag(empty_val, FLAG_EMPTY);
	return empty_val;
}

value_p new_number(double val) {
	value_p new_v = malloc(sizeof(struct value_t));
	setFlag(new_v, FLAG_NUMBER);
	setNumeric(new_v, val);
	return new_v;
}

double get_number(subrange_p p) {
	/* Assumes the calling function has
	 * already verified that subrange_p
	 * points to a single Number */
	value_p v = get_val(p, 0, 0);
	return v->numericVal;
}

value_p print(subrange_p whatever, subrange_p text) {
	if(!assertSingleString(text)) return new_val();
	value_p my_val = get_val(text,0,0);
	if(!assertText(my_val)) return new_val();
	printf("%s", my_val->str->text);
	return new_val();
}

value_p printd(subrange_p whatever, subrange_p text) {
	printf("%f\n", (*text->range->values)->numericVal);
	value_p result = malloc(sizeof(struct value_t));
	return result;
}

value_p extend_sin(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = sin(initial);
	return new_number(val);
}

value_p extend_cos(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = cos(initial);
	return new_number(val);
}

value_p extend_tan(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = tan(initial);
	return new_number(val);
}

value_p extend_asin(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = asin(initial);
	return new_number(val);
}

value_p extend_acos(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = acos(initial);
	return new_number(val);
}

value_p extend_atan(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = atan(initial);
	return new_number(val);
}

value_p extend_sinh(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = sinh(initial);
	return new_number(val);
}

value_p extend_cosh(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = cosh(initial);
	return new_number(val);
}

value_p extend_tanh(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = tanh(initial);
	return new_number(val);
}

value_p extend_exp(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = exp(initial);
	return new_number(val);
}

value_p extend_log(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = log(initial);
	return new_number(val);
}

value_p extend_log10(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = log10(initial);
	return new_number(val);
}

value_p extend_sqrt(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = sqrt(initial);
	return new_number(val);
}

value_p extend_ceil(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = ceil(initial);
	return new_number(val);
}

value_p extend_fabs(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = fabs(initial);
	return new_number(val);
}

value_p extend_floor(subrange_p range) {
	if(!assertSingleNumber(range)) return new_val();
	double initial = get_number(range);
	double val = floor(initial);
	return new_number(val);
}

value_p extend_open(subrange_p rng_filename, subrange_p rng_mode){
	FILE *val;
	if (   !assertSingleString(rng_filename)
			|| !assertSingleString(rng_mode)
			|| open_num_files + 1 > MAX_FILES) {
				return new_val();
	}
	value_p filename = get_val(rng_filename, 0, 0);
	value_p mode = get_val(rng_mode, 0,0);
	val = fopen(filename->str->text, mode->str->text);
	if(val == NULL) return new_val();
	open_num_files++;
	open_files[open_num_files] = val;
	return new_number((double) open_num_files);
}

value_p extend_close(subrange_p rng_file_handle){
	if(!assertSingleNumber(rng_file_handle)) {
		// Per the LRM this is actually supposed to crash the program.
		fprintf(stderr, "EXITING - Attempted to close something that was not a valid file pointer\n");
		exit(-1);
	}

	int fileNum = (int) get_number(rng_file_handle);
	if (fileNum > open_num_files || open_files[fileNum] == NULL) {
		// Per the LRM this is actually supposed to crash the program.
		fprintf(stderr, "EXITING - Attempted to close something that was not a valid file pointer\n");
		exit(-1);
	}
	fclose(open_files[fileNum]);
	open_files[fileNum] = NULL; // Empty the container for the pointer.
	return new_val(); // asssuming it was an open valid handle, close() is just supposed to return empty
}
/*
value_p extend_read(subrange_p rng_file_handle, subrange_p rng_num_bytes){
	/* TODO: Make it accept empty */
	/*if(!assertSingleNumber(rng_file_handle) || !assertSingleNumber(rng_num_bytes)) return new_val();
	int max_bytes = (int) get_number(rng_num_bytes);
	int fileNum = (int) get_number(rng_file_handle);
	if (fileNum > open_num_files || open_files[fileNum] == NULL)  return new_val();
	char *buf = malloc(sizeof(char) * (max_bytes + 1));
	int bytes_read = fread(buf, sizeof(char), max_bytes, open_files[fileNum]);
	buf[bytes_read] = 0;
	value_p result = box_value_string(new_string(buf));
	free(buf);
	return result;
	//edge case: how to return the entire contents of the file if n == empty?
}*/

value_p extend_write(subrange_p rng_file_handle, subrange_p buf){
	int val;
	if(!assertSingleNumber(rng_file_handle) || !assertSingleString(buf)) return new_val();
	value_p buffer = get_val(buf, 0, 0);
	int fileNum = (int) get_number(rng_file_handle);
	if (fileNum > open_num_files || open_files[fileNum] == NULL) {
		// Per the LRM this is actually supposed to crash the program.
		fprintf(stderr, "EXITING - Attempted to write to something that was not a valid file pointer\n");
		exit(-1);
	}
	fwrite(buffer->str->text, 1, buffer->str->length, open_files[fileNum]);
	// TODO: make this return empty once compiler handles Id(s)
	// RN: Use the return value to close the file
	return new_number((double) fileNum);
}

/*
 * VENDOR
 */

struct ExtendScope *global_scope;

struct var_instance *get_variable(struct ExtendScope *scope_ptr, int varnum);
value_p getVal(struct var_instance *inst, int x, int y);
void null_init(struct ExtendScope *scope_ptr) {
	int i;
	for(i = 0; i < scope_ptr->numVars; i++)
		scope_ptr->vars[i] = NULL;
}

struct var_instance *instantiate_variable(struct ExtendScope *scope_ptr, struct var_defn def) {
	printf("Hello\n");
	double rowVal, colVal;
	if(def.isOneByOne) {
		printf("OneByOne\n");
		rowVal = 1;
		colVal = 1;
	} else {
		printf("Bigger\n");
		struct var_instance *rows_var = get_variable(scope_ptr, def.rows_varnum);
		printf("Got var\n");
		fflush(stdout);
		value_p rows = getVal(rows_var,0,0);
		printf("Got val %f\n", rows->numericVal);
		fflush(stdout);
		struct var_instance *cols_var = get_variable(scope_ptr, def.cols_varnum);
		printf("Got var2 rows are %f\n", rows->numericVal);
		fflush(stdout);
		value_p cols = getVal(cols_var,0,0);
		if(rows->flags == FLAG_NUMBER || cols->flags == FLAG_NUMBER) {
			/* TODO: throw error */
		}
		rowVal = (int)(rows->numericVal + 0.5);
		colVal = (int)(cols->numericVal + 0.5);
	}
	printf("%f %f\n",rowVal, colVal);
	// TODO: do the same thing for each FormulaFP to turn an ExtendFormula into a ResolvedFormula
	struct var_instance *inst = malloc(sizeof(struct var_instance));
	inst->rows = (int)(rowVal + 0.5);
	inst->cols = (int)(colVal + 0.5);
	inst->numFormulas = def.numFormulas;
	inst->closure = scope_ptr;
	int size = inst->rows * inst->cols;
	printf("Size: %d %p\n", size, inst);
	inst->values = malloc(sizeof(value_p) * size);
	inst->status = malloc(sizeof(struct status_t) * size);
	inst->formulas = malloc(sizeof(struct ResolvedFormula) * inst->numFormulas);
	int i;
	for(i = 0; i < inst->numFormulas; i++) {
		inst->formulas[i].formula = def.formulas[i].formula;
		inst->formulas[i].colEnd = 1;
		inst->formulas[i].colStart = 0;
		inst->formulas[i].rowEnd = 1;
		inst->formulas[i].rowStart = 0;
	}
	for(i = 0; i < inst->rows * inst->cols; i++)
		(*inst->status) = 0;
	return inst;
}

struct var_instance *get_variable(struct ExtendScope *scope_ptr, int varnum) {
	if (varnum >= scope_ptr->numVars) {
		fprintf(stderr, "Runtime error: Asked for nonexistant variable number\n");
		exit(-1);
	}
	printf("%p\n", scope_ptr->defns);
	printf("Num vars: %d, target: %d\n", scope_ptr->numVars, varnum);
	if (scope_ptr->vars[varnum] == NULL) {
		printf("A\n");
		scope_ptr->vars[varnum] = instantiate_variable(scope_ptr, scope_ptr->defns[varnum]);
	}
	printf("B: %p, ", scope_ptr->vars[varnum]);
	fflush(stdout);
	printf("%d\n", scope_ptr->vars[varnum]->cols);
	return scope_ptr->vars[varnum];
}

bool assertInBounds(struct var_instance *defn, int x, int y) {
	if(defn->rows > x && defn->cols > y) return true;
	return false;
}

bool fitsDim(int dim, int rowStart_varnum, int rowEnd_varnum) {
	return (dim >= rowStart_varnum) && (dim <= rowEnd_varnum);
}

bool fitsRange(struct ResolvedFormula *formula, int x, int y) {
	return fitsDim(x, formula->colStart, formula->colEnd)
		&& fitsDim(y, formula->rowStart, formula->rowEnd);
}

value_p calcVal(struct var_instance *inst, int x, int y, value_p target) {
	struct ResolvedFormula *form = inst->formulas;
	while(form < inst->formulas + inst->numFormulas) {
		if(fitsRange(form, x, y)) {
				value_p res = (form->formula)(inst->closure, x, y);
				return res;
		}
		form++;
	}
	return new_val();
}

value_p getVal(struct var_instance *inst, int x, int y) {
	printf("Bye %d %d %d %d\n", inst->cols, inst->rows, x, y);
	if(!assertInBounds(inst, x, y)) return new_val();
	printf("Bye2 %p\n", inst);
	int offset = inst->rows * y + x;
	printf("Offset: %d %p\n", offset, inst->status);
	char *status = inst->status + offset;
	printf("Stat %p %d %d %d\n", status, (int)*status, *status & IN_PROGRESS, ~(*status));
	if(*status & IN_PROGRESS) {
		/* TODO: Circular dependency. Possibly throw? */
		printf("Why?\n");
		fflush(stdout);
		return new_val();
	} else if ((~(*status)) & CALCULATED) { /* value not calculated */
		printf("Calc val...\n");
		fflush(stdout);
		value_p val = calcVal(inst, x, y, inst->values[offset]);
			printf("Calc val...\n");
			fflush(stdout);
		inst->values[offset] = val;
		*status = (*status && !IN_PROGRESS) | CALCULATED;
		printf("Calculated: %f\n", inst->values[offset]->numericVal);
		return val;
	} else {
		printf("Cached: %f\n", inst->values[offset]->numericVal);
		fflush(stdout);
		return inst->values[offset];
	}
}
