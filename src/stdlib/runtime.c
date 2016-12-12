#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<stdbool.h>
#include "runtime.h"

double setNumeric(value_p result, double val) {
	result->flags = FLAG_NUMBER;
	return (result->numericVal = val);
}

char* setString(value_p result, char *str, int length) {
	result->flags = FLAG_STRING;
	result->str = malloc(sizeof(struct string_t));
  result->str->length = length;
	return (result->str->text = str);
}

double setFlag(value_p result, double flag_num) {
	return (result->flags = flag_num);
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


struct ExtendScope *global_scope;

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
		rowVal = (int)lrint(rows->numericVal);
		colVal = (int)lrint(cols->numericVal);
	}
	// TODO: do the same thing for each FormulaFP to turn an ExtendFormula into a ResolvedFormula
	struct var_instance *inst = malloc(sizeof(struct var_instance));
	inst->rows = (int)lrint(rowVal);
	inst->cols = (int)lrint(colVal);
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

	scope_ptr->refcount++;
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

void setRange(value_p val, struct var_instance *inst) {
	subrange_p sr = malloc(sizeof(struct subrange_t));
	sr->offsetCol = 0;
	sr->offsetRow = 0;
	sr->subrangeCol = inst->cols;
	sr->subrangeRow = inst->rows;
	sr->range = inst;
	val->subrange = sr;
	val->flags = FLAG_SUBRANGE;
}

value_p getSize(struct var_instance *inst) {
	value_p res = malloc(sizeof(struct value_t));
	setNumeric(res, 1); /*TODO*/
	return res;
}

value_p deepCopy(value_p value) {
	value_p _new = new_val();
	if(value->flags == FLAG_EMPTY) {}
	else if(value->flags == FLAG_STRING) {
		_new->flags = FLAG_STRING;
		_new->str = malloc(sizeof(struct string_t));
		memcpy(_new->str->text, value->str->text, value->str->length);
		_new->str->length = value->str->length;
	}
	else if(value->flags == FLAG_NUMBER) {
		_new->flags = FLAG_NUMBER;
		_new->numericVal = value->numericVal;
	}
	else if(value->flags == FLAG_SUBRANGE) {
		struct var_instance *v = malloc(sizeof(struct subrange_t));
		int cols = value->subrange->subrangeCol;
		int rows = value->subrange->subrangeRow;
		v->name = "COPYCAT";
		v->formulas = NULL;
		v->status = malloc(sizeof(char *) * rows * cols);
		v->values = malloc(sizeof(value_p) * rows * cols);
		v->closure = NULL;
		int i,j;
		for(i = 0; i < rows; i++) {
			for(j = 0; j < cols; j++) {
				int offset = i * rows + j;
				*(v->status + offset) = CALCULATED;
				/*TODO: eval lazzzy*/
				*(v->values + offset) = getVal(value->subrange->range, i + value->subrange->offsetRow, j + value->subrange->offsetCol);
			}
		}
		setRange(_new, v);
	}
	return _new;
}

value_p clone_value(value_p old_value) {
	value_p new_value = (value_p) malloc(sizeof(struct value_t));
	new_value->flags = old_value->flags;
	switch (new_value->flags) {
		case FLAG_EMPTY:
			break;
		case FLAG_NUMBER:
			new_value->numericVal = old_value->numericVal;
			break;
		case FLAG_STRING:
			new_value->str = old_value->str;
			new_value->str->refs++;
			break;
		case FLAG_SUBRANGE:
			new_value->subrange = (subrange_p) malloc(sizeof(struct subrange_t));
			memcpy(new_value->subrange, old_value->subrange, sizeof(struct subrange_t));
			new_value->subrange->range->closure->refcount++; /* Not sure about this one */
			break;
		default:
			fprintf(stderr, "clone_value(%p): Illegal value of flags: %c\n", old_value, new_value->flags);
			exit(-1);
			break;
	}
	return new_value;
}

void delete_string_p(string_p old_string) {
	old_string->refs--;
	if (old_string->refs == 0) {
		/* free(old_string); */
	}
}

void delete_subrange_p(subrange_p old_subrange) {
	old_subrange->range->closure->refcount--;
	free(old_subrange);
}

void delete_value(value_p old_value) {
	switch (old_value->flags) {
		case FLAG_EMPTY:
			break;
		case FLAG_NUMBER:
			break;
		case FLAG_STRING:
			delete_string_p(old_value->str); /* doesn't do anything besides decrement the ref count now */
			break;
		case FLAG_SUBRANGE:
			delete_subrange_p(old_value->subrange);
			break;
		default:
			fprintf(stderr, "delete_value(%p): Illegal value of flags: %c\n", old_value, old_value->flags);
			exit(-1);
			break;
	}
}

value_p getVal(struct var_instance *inst, int x, int y) {
	if(!assertInBounds(inst, x, y)) return new_val();
	int offset = inst->rows * y + x;
	char *status = inst->status + offset;
	value_p return_val;
	if(*status & IN_PROGRESS) {
		/* TODO: Circular dependency. Possibly throw? */
		return_val = new_val();
	} else if ((~(*status)) & CALCULATED) { /* value not calculated */
		value_p val = calcVal(inst, x, y);
		inst->values[offset] = val;
		*status = (*status && !IN_PROGRESS) | CALCULATED;
		return_val = val;
	} else {
		return_val = inst->values[offset];
	}
	while(return_val->flags == FLAG_SUBRANGE && return_val->subrange->subrangeRow == 1 && return_val->subrange->subrangeCol == 1) {
		return_val = getVal(return_val->subrange->range, return_val->subrange->offsetRow, return_val->subrange->offsetCol);
	}
	return return_val;
}
