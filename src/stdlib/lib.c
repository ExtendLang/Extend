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
	 char *name;
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
	char *name;
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

value_p getVal(struct var_instance *inst, int x, int y);

value_p __get_val(struct var_instance *range, int row, int col) {
	//TODO: assertions
	printf("Getting %p %d %d\n", range, row, col);
	return getVal(range, row, col);
	/*
	value_p val = range->values[row * range->cols + col];
	return val;*/
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

int assertSingle(value_p value) {
	/* TODO: dereference 1 by 1 subrange */
	return !(value->flags == FLAG_SUBRANGE);
}

int assertSingleNumber(value_p p) {
	if (!assertSingle(p)) {
		return 0;
	}
	return (p->flags == FLAG_NUMBER);
}

int assertText(value_p my_val) {
	return (my_val->flags == FLAG_STRING);
}

int assertSingleString(value_p p) {
	if (!assertSingle(p)) {
		return 0;
	}
	return (p->flags == FLAG_STRING);
}

int assertEmpty(value_p p) {
	if (!assertSingle(p)) {
		return 0;
	}
	return (p->flags == FLAG_EMPTY);
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


value_p printv(value_p whatever, value_p text) {
	printf("%s", text->str->text);
	return new_val();
}


value_p printd(value_p whatever, value_p text) {
	printf("%f\n", text->numericVal);
	value_p result = malloc(sizeof(struct value_t));
	return result;
}

#define FUNC(name) value_p extend_##name(value_p a){if(!assertSingleNumber(a)) return new_val();double val = name(a->numericVal);return new_number(val);}
FUNC(sin)
FUNC(cos)
FUNC(tan)
FUNC(acos)
FUNC(asin)
FUNC(atan)
FUNC(sinh)
FUNC(cosh)
FUNC(tanh)
FUNC(exp)
FUNC(log)
FUNC(log10)
FUNC(sqrt)
FUNC(ceil)
FUNC(fabs)
FUNC(floor)

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
/*<<<<<<< HEAD
	if(!assertSingleNumber(rng_file_handle) || !assertSingleNumber(rng_num_bytes)) return new_val();
	int max_bytes = (int) get_number(rng_num_bytes);
	int fileNum = (int) get_number(rng_file_handle);
=======
	if(!assertSingleNumber(rng_file_handle) || !assertSingleNumber(rng_num_bytes)) return new_val();
	int fileNum = (int) get_number(rng_file_handle), max_bytes;
>>>>>>> finish-transformations
	if (fileNum > open_num_files || open_files[fileNum] == NULL)  return new_val();
	FILE *f = open_files[fileNum];
	max_bytes = (int) get_number(rng_num_bytes);
	if (max_bytes == 0) {
		long cur_pos = ftell(f);
		fseek(f, 0, SEEK_END);
		long end_pos = ftell(f);
		fseek(f, cur_pos, SEEK_SET);
		max_bytes = end_pos - cur_pos;
	}
	char *buf = malloc(sizeof(char) * (max_bytes + 1));
	int bytes_read = fread(buf, sizeof(char), max_bytes, f);
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
void null_init(struct ExtendScope *scope_ptr) {
	int i;
	for(i = 0; i < scope_ptr->numVars; i++)
		scope_ptr->vars[i] = NULL;
}

struct var_instance *instantiate_variable(struct ExtendScope *scope_ptr, struct var_defn def) {
	double rowVal, colVal;
	if(def.isOneByOne) {
		rowVal = 1;
		colVal = 1;
	} else {
		struct var_instance *rows_var = get_variable(scope_ptr, def.rows_varnum);
		fflush(stdout);
		value_p rows = getVal(rows_var,0,0);
		fflush(stdout);
		struct var_instance *cols_var = get_variable(scope_ptr, def.cols_varnum);
		fflush(stdout);
		value_p cols = getVal(cols_var,0,0);
		if(rows->flags == FLAG_NUMBER || cols->flags == FLAG_NUMBER) {
			/* TODO: throw error */
		}
		rowVal = (int)(rows->numericVal + 0.5);
		colVal = (int)(cols->numericVal + 0.5);
	}
	// TODO: do the same thing for each FormulaFP to turn an ExtendFormula into a ResolvedFormula
	struct var_instance *inst = malloc(sizeof(struct var_instance));
	inst->rows = (int)(rowVal + 0.5);
	inst->cols = (int)(colVal + 0.5);
	inst->numFormulas = def.numFormulas;
	inst->closure = scope_ptr;
	inst->name = def.name;
	int size = inst->rows * inst->cols;
	inst->values = malloc(sizeof(value_p) * size);
	inst->status = malloc(sizeof(struct status_t) * size);
	inst->formulas = malloc(sizeof(struct ResolvedFormula) * inst->numFormulas);
	int i;
	for(i = 0; i < inst->numFormulas; i++) {
		inst->formulas[i].formula = def.formulas[i].formula;
		if(def.formulas[i].fromFirstRow)
			inst->formulas[i].rowStart = 0;
		else
			inst->formulas[i].rowStart = def.formulas[i].rowStart_varnum; //TODO eval;
		if(def.formulas[i].toLastRow)
			inst->formulas[i].rowEnd = inst->rows;
		else
			inst->formulas[i].rowEnd = def.formulas[i].rowEnd_varnum; //TODO eval;
		if(def.formulas[i].fromFirstCol)
			inst->formulas[i].colStart = 0;
		else
			inst->formulas[i].colStart = def.formulas[i].colStart_varnum; //TODO eval;
		if(def.formulas[i].toLastCol)
			inst->formulas[i].colEnd = 0;
		else
			inst->formulas[i].colEnd = def.formulas[i].colEnd_varnum; //TODO eval;
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
	if (scope_ptr->vars[varnum] == NULL) {
		scope_ptr->vars[varnum] = instantiate_variable(scope_ptr, scope_ptr->defns[varnum]);
	}
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

value_p calcVal(struct var_instance *inst, int x, int y) {
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

value_p getSize(struct var_instance *inst) {
	value_p res = malloc(sizeof(struct value_t));
	setNumeric(res, 1); /*TODO*/
	return res;
}

value_p getVal(struct var_instance *inst, int x, int y) {
	if(!assertInBounds(inst, x, y)) return new_val();
	int offset = inst->rows * y + x;
	char *status = inst->status + offset;
	if(*status & IN_PROGRESS) {
		/* TODO: Circular dependency. Possibly throw? */
		return new_val();
	} else if ((~(*status)) & CALCULATED) { /* value not calculated */
		value_p val = calcVal(inst, x, y);
		inst->values[offset] = val;
		*status = (*status && !IN_PROGRESS) | CALCULATED;
		return val;
	} else {
		return inst->values[offset];
	}
}
