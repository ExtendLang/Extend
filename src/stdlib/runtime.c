#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<stdbool.h>
#include "runtime.h"

void debug_print(value_p val, char *which_value) {
	char *flag_meanings[4] = {"Empty", "Number", "String", "Subrange"};
	fprintf(stderr, "------Everything you ever wanted to know about %s:------\n", which_value == NULL ? "some anonymous variable" : which_value);
	fprintf(stderr, "Memory address: %p\n", val);
	if (val == NULL) {
		fprintf(stderr, "------------Nice try asking me to dereference a null pointer\n------------");
		return;
	}
	fprintf(stderr, "Flags: %d (%s)\n", val->flags, flag_meanings[val->flags]);
	fprintf(stderr, "NumericVal: %f\n", val->numericVal);
	fprintf(stderr, "String contents: Probably safer not to check that pointer (%p) blindly\n", val->str);
	if (val->flags == FLAG_STRING && val->str != NULL) {
		fprintf(stderr, "It says it's a string and it's not a NULL pointer though, so here you go:\n");
		fprintf(stderr, "String refcount: %d\n", val->str->refs);
		fprintf(stderr, "String length: %ld\n", val->str->length);
		fprintf(stderr, "String char* memory address: %p\n", val->str->text);
		if (val->str->text == NULL) {
			fprintf(stderr, "Not going to print the contents of NULL!\n");
		} else {
			fprintf(stderr, "String char* contents:\n%s\n", val->str->text);
		}
	}
	fprintf(stderr, "Subrange contents: Probably safer not to check that pointer (%p) blindly either\n", val->subrange);
	fprintf(stderr, "------That's all I've got to say about %s:------\n", which_value == NULL ? "some anonymous variable" : which_value);
}

void debug_print_formula(struct ExtendFormula *fdef) {
	fprintf(stderr, "------Everything you ever wanted to know about your favorite formula:------\n");
	fprintf(stderr, "RowStart varnum: %d %d\n", fdef->rowStart_varnum, fdef->fromFirstRow);
	fprintf(stderr, "RowEnd varnum: %d %d\n", fdef->rowEnd_varnum, fdef->toLastRow);
	fprintf(stderr, "ColStart varnum: %d %d\n", fdef->colStart_varnum, fdef->fromFirstCol);
	fprintf(stderr, "ColEnd varnum: %d %d\n", fdef->colEnd_varnum, fdef->toLastCol);
}

void debug_print_res_formula(struct ResolvedFormula *rdef) {
	fprintf(stderr, "Some formula applies to: [%d:%d,%d:%d]\n", rdef->rowStart, rdef->rowEnd, rdef->colStart, rdef->colEnd);
}

void debug_print_vardefn(struct var_defn *pdef) {
	fprintf(stderr, "------Everything you ever wanted to know about var defn %s:------\n", pdef->name);
	fprintf(stderr, "Row varnum: %d\n", pdef->rows_varnum);
	fprintf(stderr, "Col varnum: %d\n", pdef->cols_varnum);
	fprintf(stderr, "Num formulas: %d\n", pdef->numFormulas);
	fprintf(stderr, "Formula defs: \n");
	int i;
	for (i=0; i < pdef->numFormulas; i++) {
		debug_print_formula(pdef->formulas + i);
	}
	fprintf(stderr, "Is 1x1: %d\n", pdef->isOneByOne);
}

void debug_print_varinst(struct var_instance *inst) {
	fprintf(stderr, "------Everything you ever wanted to know about var %s:------\n", inst->name);
	fprintf(stderr, "Rows: %d\n", inst->rows);
	fprintf(stderr, "Cols: %d\n", inst->cols);
	fprintf(stderr, "Num formulas: %d\n", inst->numFormulas);
	fprintf(stderr, "*****Formulas:*****\n");
	int i;
	for (i = 0; i < inst->numFormulas; i++) {
		debug_print_res_formula(inst->formulas + i);
	}
	fprintf(stderr, "**** End of Formulas *** \n");
	fprintf(stderr, "*****Values:*****\n");
	for (i=0; i < inst->rows * inst->cols; i++) {
		debug_print(inst->values[i], inst->name);
	}
	fprintf(stderr, "**** End of Values *** \n");
}

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

value_p new_string_go_all_the_way(char *s) {
	if (s == NULL) return new_val();
	value_p new_v = malloc(sizeof(struct value_t));
	setFlag(new_v, FLAG_STRING);
	string_p new_str = malloc(sizeof(struct string_t));
	long len = strlen(s);
	new_str->text = malloc(len+1);
	strcpy(new_str->text, s);
	new_str->length = len;
	new_str->refs = 1;
	new_v->str = new_str;
	return new_v;
}

struct ExtendScope *global_scope;

void null_init(struct ExtendScope *scope_ptr) {
	int i;
	for(i = 0; i < scope_ptr->numVars; i++)
		scope_ptr->vars[i] = NULL;
}

int getIntFromOneByOne(struct ExtendScope *scope_ptr, int varnum) {
	struct var_instance *inst = get_variable(scope_ptr, varnum);
	if (inst->rows != 1 || inst->cols != 1) {
		fprintf(stderr, "The variable you claimed (%s) was one by one is actually %d by %d.\n", inst->name, inst->rows, inst->cols);
		exit(-1);
	}
	value_p val = getVal(inst, 0, 0);
	if (!assertSingleNumber(val)) {
		fprintf(stderr, "The variable you claimed (%s) was a number isn't.\n", inst->name);
		debug_print(val, inst->name);
		exit(-1);
	}
	return (int) lrint(val->numericVal);
}

struct var_instance *instantiate_variable(struct ExtendScope *scope_ptr, struct var_defn def) {
	struct var_instance *inst = malloc(sizeof(struct var_instance));
	if(def.isOneByOne) {
		inst->rows = 1;
		inst->cols = 1;
	} else {
		inst->rows = getIntFromOneByOne(scope_ptr, def.rows_varnum);
		inst->cols = getIntFromOneByOne(scope_ptr, def.cols_varnum);
	}
	// TODO: do the same thing for each FormulaFP to turn an ExtendFormula into a ResolvedFormula
	inst->numFormulas = def.numFormulas;
	inst->closure = scope_ptr;
	inst->name = def.name;
	int size = inst->rows * inst->cols;
	inst->values = malloc(sizeof(value_p) * size);
	inst->status = malloc(sizeof(char) * size);
	inst->formulas = malloc(sizeof(struct ResolvedFormula) * inst->numFormulas);
	//debug_print_vardefn(&def);
	//debug_print_varinst(inst);
	int i;
	for(i = 0; i < inst->numFormulas; i++) {

		// Set the formula function pointer to the pointer from the definition
		inst->formulas[i].formula = def.formulas[i].formula;

		if (def.isOneByOne) {
			inst->formulas[i].rowStart = 0;
			inst->formulas[i].rowEnd = 1;
			inst->formulas[i].colStart = 0;
			inst->formulas[i].colEnd = 1;
		} else {
			if(def.formulas[i].fromFirstRow) {
				inst->formulas[i].rowStart = 0;
			} else {
				inst->formulas[i].rowStart = getIntFromOneByOne(scope_ptr, def.formulas[i].rowStart_varnum);
				if (inst->formulas[i].rowStart < 0) {
					inst->formulas[i].rowStart += inst->rows;
				}
				if (inst->formulas[i].rowStart < 0 || inst->formulas[i].rowStart >= inst->rows) {
					//Doesn't matter, but will never get called
				}
				if (def.formulas[i].isSingleRow) {
					inst->formulas[i].rowEnd = inst->formulas[i].rowStart + 1;
				} else if (def.formulas[i].toLastRow) {
					inst->formulas[i].rowEnd = inst->rows;
				} else {
					inst->formulas[i].rowEnd = getIntFromOneByOne(scope_ptr, def.formulas[i].rowEnd_varnum);
				}
			}
			if(def.formulas[i].fromFirstCol) {
				inst->formulas[i].colStart = 0;
			} else {
				inst->formulas[i].colStart = getIntFromOneByOne(scope_ptr, def.formulas[i].colStart_varnum);
				if (inst->formulas[i].colStart < 0) {
					inst->formulas[i].colStart += inst->cols;
				}
				if (inst->formulas[i].colStart < 0 || inst->formulas[i].colStart >= inst->cols) {
					//Doesn't matter, but will never get called
				}
				if (def.formulas[i].isSingleCol) {
					inst->formulas[i].colEnd = inst->formulas[i].colStart + 1;
				} else if (def.formulas[i].toLastCol) {
					inst->formulas[i].colEnd = inst->cols;
				} else {
					inst->formulas[i].colEnd = getIntFromOneByOne(scope_ptr, def.formulas[i].colEnd_varnum);
				}
			}
		}
	}
	for(i = 0; i < inst->rows * inst->cols; i++)
		*(inst->status + i) = 0;

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

char assertInBounds(struct var_instance *defn, int x, int y) {
	if(defn->rows > x && defn->cols > y) return true;
	return false;
}

char fitsDim(int dim, int rowStart_varnum, int rowEnd_varnum) {
	return (dim >= rowStart_varnum) && (dim <= rowEnd_varnum);
}

char fitsRange(struct ResolvedFormula *formula, int r, int c) {
	return fitsDim(r, formula->rowStart, formula->rowEnd)
		&& fitsDim(c, formula->colStart, formula->colEnd);
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
//	debug_print_varinst(inst);
	return return_val;
}
