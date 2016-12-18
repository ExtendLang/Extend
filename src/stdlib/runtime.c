#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<sys/resource.h>
#include<string.h>
#include<stdbool.h>
#include "runtime.h"

struct value_t zero_val = {FLAG_NUMBER, 0.0, NULL, NULL};
struct value_t one_val = {FLAG_NUMBER, 1.0, NULL, NULL};
struct rhs_index absolute_zero = {&zero_val, RHS_IDX_ABSOLUTE};
struct rhs_index absolute_one = {&one_val, RHS_IDX_ABSOLUTE};
struct rhs_slice zero_to_one = {&absolute_zero, &absolute_one};
struct rhs_slice corresponding_cell = {NULL, NULL};

void debug_print_subrange(subrange_p subrng);

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
	if (val->flags == FLAG_SUBRANGE && val->subrange != NULL) {
		fprintf(stderr, "It says it's a subrange and it's not a NULL pointer though, so here you go:\n");
		debug_print_subrange(val->subrange);
	}
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
	fprintf(stderr, "Some formula with function pointer %p applies to: [%d:%d,%d:%d]\n", rdef->formula, rdef->rowStart, rdef->rowEnd, rdef->colStart, rdef->colEnd);
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
	fprintf(stderr, "~~~~~~~~Cells:~~~~~~~\n");
	fprintf(stderr, "Status memory address: %p\n", inst->status);
	for (i = 0; i < inst->rows * inst->cols; i++) {
		printf("%s[%d,%d]: Status=%d\n", inst->name, i / inst->cols, i % inst->cols, inst->status[i]);
		if (inst->status[i] == CALCULATED) {
			printf("%s[%d,%d] Value:\n", inst->name, i / inst->cols, i % inst->cols);
			debug_print(inst->values[i], inst->name);
		}
	}
	fprintf(stderr, "~~~ End of Cells: ~~~\n");
}

void debug_print_subrange(subrange_p subrng) {
	fprintf(stderr, "-------Everything you wanted to know about this subrange------\n");
	fprintf(stderr, "Offset: [%d,%d]\n", subrng->base_var_offset_row, subrng->base_var_offset_col);
	fprintf(stderr, "Dimensions: [%d,%d]\n", subrng->subrange_num_rows, subrng->subrange_num_cols);
	fprintf(stderr, "Subrange of: \n");
	debug_print_varinst(subrng->range);
}

void debug_print_index(struct rhs_index *idx) {
	if (idx == NULL) {
		fprintf(stderr, "I'd rather not try to print out the contents of a NULL index.\n");
		exit(-1);
	}
	fprintf(stderr, "Index type: ");
	switch(idx->rhs_index_type) {
		case RHS_IDX_ABSOLUTE:
			fprintf(stderr, "Absolute\n");
			if (idx->val_of_expr == NULL) {
				fprintf(stderr, "I wasn't expecting this, but the value pointer is NULL. Maybe there's a good reason for it, so I'll keep going...\n");
			} else {
				debug_print(idx->val_of_expr, "an absolute index");
			}
			break;
		case RHS_IDX_RELATIVE:
			fprintf(stderr, "Relative\n");
			if (idx->val_of_expr == NULL) {
				fprintf(stderr, "I wasn't expecting this, but the value pointer is NULL. Maybe there's a good reason for it, so I'll keep going...\n");
			} else {
				debug_print(idx->val_of_expr, "a relative index");
			}
			break;
		case RHS_IDX_DIM_START:
			fprintf(stderr, "DimensionStart\n");
			if (idx->val_of_expr != NULL) {
				fprintf(stderr, "This definitely isn't supposed to happen - the value pointer isn't NULL. You should look into that.\n");
				exit(-1);
			}
			break;
		case RHS_IDX_DIM_END:
			fprintf(stderr, "DimensionEnd\n");
			if (idx->val_of_expr != NULL) {
				fprintf(stderr, "This definitely isn't supposed to happen - the value pointer isn't NULL. You should look into that.\n");
				exit(-1);
			}
			break;
	}
}

void debug_print_slice(struct rhs_slice *sl) {
	if (sl == NULL) {
		fprintf(stderr, "I'd rather not try to print out the contents of a NULL slice.\n");
		exit(-1);
	}
	fprintf(stderr, "-------Everything about this slice------\n");
	fprintf(stderr, "Start and end index memory addresses: %p and %p\n", sl->slice_start_index, sl->slice_end_index);
	if (sl->slice_start_index != NULL) {
		fprintf(stderr, "Start index info:\n");
		debug_print_index(sl->slice_start_index);
		if (sl->slice_end_index != NULL) {
			fprintf(stderr, "End index info:\n");
			debug_print_index(sl->slice_end_index);
		}
	}	else {
		if (sl->slice_end_index != NULL) {
			fprintf(stderr, "Start index is NULL but end index is not NULL. That should never happen.\n");
			fprintf(stderr, "Attempting to print contents anyway:\n");
			fflush(stderr);
			debug_print_index(sl->slice_end_index);
		}
	}
}

void debug_print_selection(struct rhs_selection *sel) {
	if (sel == NULL) {
		fprintf(stderr, "I'd rather not try to print out the contents of a NULL selection.\n");
		exit(-1);
	}
	fprintf(stderr, "-------Everything about this selection------\n");
	fprintf(stderr, "Slice memory addresses: %p and %p\n", sel->slice1, sel->slice2);
	if (sel->slice1 != NULL) {
		fprintf(stderr, "Slice 1 info:\n");
		debug_print_slice(sel->slice1);
		if (sel->slice2 != NULL) {
			fprintf(stderr, "Slice 2 info:\n");
			debug_print_slice(sel->slice2);
		}
	}	else {
		if (sel->slice2 != NULL) {
			fprintf(stderr, "Slice 1 is NULL but slice 2 is not NULL. That should never happen.\n");
			fprintf(stderr, "Attempting to print contents anyway:\n");
			fflush(stderr);
			debug_print_slice(sel->slice2);
		}
	}
	fprintf(stderr, "-------That's all I've got about that selection------\n\n");
}

int rg_eq(value_p val1, value_p val2) {
	int res = 1;
	if(val1->flags != val2->flags) res = 0;
	else if(val1->flags == FLAG_EMPTY) ;
	else if(val1->flags == FLAG_NUMBER && val1->numericVal != val2->numericVal) res = 0;
	else if(val1->flags == FLAG_STRING && strcmp(val1->str->text, val2->str->text)) res = 0;
	else if(val1->flags == FLAG_SUBRANGE) {
		subrange_p sr1 = val1->subrange;
		subrange_p sr2 = val2->subrange;
		if(sr1->subrange_num_cols != sr2->subrange_num_cols || sr1->subrange_num_rows != sr2->subrange_num_rows) {
			return 0;
		} else {
			int i, j;
			value_p v1, v2;
			for(i = 0; i < sr1->subrange_num_rows; i++) {
				for(j = 0; j < sr1->subrange_num_cols; j++) {
					v1 = getValSR(sr1, i, j);
					v2 = getValSR(sr2, i, j);
					if(rg_eq(v1, v2) == 0) {
						return 0;
					}
				}
			}
		}
	}
	return res;
}

void incStack() {
	const rlim_t kStackSize = 64L * 1024L * 1024L;
	struct rlimit rl;
	int result;

	result = getrlimit(RLIMIT_STACK, &rl);
  rl.rlim_cur = rl.rlim_max;
  result = setrlimit(RLIMIT_STACK, &rl);
}

double setNumeric(value_p result, double val) {
	result->flags = FLAG_NUMBER;
	return (result->numericVal = val);
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

value_p new_string(char *s) {
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

char getIntFromOneByOne(struct ExtendScope *scope_ptr, int varnum, int *result) {
	if (!scope_ptr->defns[varnum].isOneByOne) {
		fprintf(stderr, "A variable (%s) that is supposedly one by one is not defined that way.\n", scope_ptr->defns[varnum].name);
		exit(-1);
	}
	struct var_instance *inst = get_variable(scope_ptr, varnum);
	if (inst->rows != 1 || inst->cols != 1) {
		fprintf(stderr, "A variable (%s) that is defined as one by one is somehow actually %d by %d.\n", inst->name, inst->rows, inst->cols);
		exit(-1);
	}
	value_p val = getVal(inst, 0, 0);
	if (!assertSingleNumber(val) || !isfinite(val->numericVal)) {
		return 0;
	}
	*result = (int) lrint(val->numericVal);
	return 1;
}

struct var_instance *instantiate_variable(struct ExtendScope *scope_ptr, struct var_defn def) {
	struct var_instance *inst = malloc(sizeof(struct var_instance));
	if(def.isOneByOne) {
		inst->rows = 1;
		inst->cols = 1;
	} else {
		if (!getIntFromOneByOne(scope_ptr, def.rows_varnum, &inst->rows)) {
			fprintf(stderr, "EXITING - The expression for the number of rows of variable %s did not evaluate to a finite Number.\n", def.name);
			exit(-1);
		}
		if (!getIntFromOneByOne(scope_ptr, def.cols_varnum, &inst->cols)) {
			fprintf(stderr, "EXITING - The expression for the number of columns of variable %s did not evaluate to a finite Number.\n", def.name);
			exit(-1);
		}
		if (inst->rows <= 0 || inst->cols <= 0) {
			fprintf(stderr, "EXITING - The requested dimensions for variable %s were [%d, %d]; they must both be greater than zero.\n", def.name, inst->rows, inst->cols);
			exit(-1);
		}
	}
	// TODO: do the same thing for each FormulaFP to turn an ExtendFormula into a ResolvedFormula
	inst->numFormulas = def.numFormulas;
	inst->closure = scope_ptr;
	inst->name = def.name;
	int size = inst->rows * inst->cols;
	inst->values = malloc(sizeof(value_p) * size);
	memset(inst->values, 0, sizeof(value_p) * size);
	inst->status = malloc(sizeof(char) * size);
	memset(inst->status, 0, sizeof(char) * size);
	inst->formulas = malloc(sizeof(struct ResolvedFormula) * inst->numFormulas);
	//debug_print_vardefn(&def);
	//debug_print_varinst(inst);
	int i, j;
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
				if (!getIntFromOneByOne(scope_ptr, def.formulas[i].rowStart_varnum, &inst->formulas[i].rowStart)) {
					fprintf(stderr, "EXITING - The requested starting row for formula %d of %s did not evaluate to a finite number.\n", i, inst->name);
					exit(-1);
				}
				if (inst->formulas[i].rowStart < 0) {
					inst->formulas[i].rowStart += inst->rows;
				}
				if (inst->formulas[i].rowStart < 0 || inst->formulas[i].rowStart >= inst->rows) {
					//Doesn't matter, but will never get called
				}
			}
			if (def.formulas[i].isSingleRow) {
				inst->formulas[i].rowEnd = inst->formulas[i].rowStart + 1;
			} else if (def.formulas[i].toLastRow) {
				inst->formulas[i].rowEnd = inst->rows;
			} else {
				if (!getIntFromOneByOne(scope_ptr, def.formulas[i].rowEnd_varnum, &inst->formulas[i].rowEnd)) {
					fprintf(stderr, "EXITING - The requested ending row for formula %d of %s did not evaluate to a finite number.\n", i, inst->name);
					exit(-1);
				}
				if (inst->formulas[i].rowEnd < 0) {
					inst->formulas[i].rowEnd += inst->rows;
				}
			}
			if(def.formulas[i].fromFirstCol) {
				inst->formulas[i].colStart = 0;
			} else {
				if (!getIntFromOneByOne(scope_ptr, def.formulas[i].colStart_varnum, &inst->formulas[i].colStart)) {
					fprintf(stderr, "EXITING - The requested starting column for formula %d of %s did not evaluate to a finite number.\n", i, inst->name);
					exit(-1);
				}
				if (inst->formulas[i].colStart < 0) {
					inst->formulas[i].colStart += inst->cols;
				}
				if (inst->formulas[i].colStart < 0 || inst->formulas[i].colStart >= inst->cols) {
					//Doesn't matter, but will never get called
				}
			}
			if (def.formulas[i].isSingleCol) {
				inst->formulas[i].colEnd = inst->formulas[i].colStart + 1;
			} else if (def.formulas[i].toLastCol) {
				inst->formulas[i].colEnd = inst->cols;
			} else {
				if (!getIntFromOneByOne(scope_ptr, def.formulas[i].colEnd_varnum, &inst->formulas[i].colEnd)) {
					fprintf(stderr, "EXITING - The requested starting column for formula %d of %s did not evaluate to a finite number.\n", i, inst->name);
					exit(-1);
				}
				if (inst->formulas[i].colEnd < 0) {
					inst->formulas[i].colEnd += inst->cols;
				}
			}
		}
	}

	for (i = 1; i < inst->numFormulas; i++) {
		for (j = 0; j < i; j++) {
			int intersectRowStart = (inst->formulas[i].rowStart > inst->formulas[j].rowStart) ? inst->formulas[i].rowStart : inst->formulas[j].rowStart;
			int intersectColStart = (inst->formulas[i].colStart > inst->formulas[j].colStart) ? inst->formulas[i].colStart : inst->formulas[j].colStart;
			int intersectRowEnd = (inst->formulas[i].rowEnd < inst->formulas[j].rowEnd) ? inst->formulas[i].rowEnd : inst->formulas[j].rowEnd;
			int intersectColEnd = (inst->formulas[i].colEnd < inst->formulas[j].colEnd) ? inst->formulas[i].colEnd : inst->formulas[j].colEnd;
			if (intersectRowEnd > intersectRowStart && intersectColEnd > intersectColStart) {
				fprintf(stderr, "Runtime error: Multiple formulas were assigned to %s[%d:%d,%d:%d].\n", inst->name,
												intersectRowStart, intersectRowEnd, intersectColStart, intersectColEnd);
				exit(-1);
			}
		}
	}

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

char assertInBounds(struct var_instance *defn, int r, int c) {
	return (
		r >= 0 && r < defn->rows &&
		c >= 0 && c < defn->cols
	);
}

value_p calcVal(struct var_instance *inst, int r, int c) {
	int i;
	for (i = 0; i < inst->numFormulas; i++) {
		if (
			r >= inst->formulas[i].rowStart && r < inst->formulas[i].rowEnd &&
			c >= inst->formulas[i].colStart && c < inst->formulas[i].colEnd
		) {
			return (inst->formulas[i].formula)(inst->closure, r, c);
		}
	}
	return new_val();
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
			if (new_value->subrange->range->closure != NULL) {
				new_value->subrange->range->closure->refcount++; /* Not sure about this one */
			}
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
	if (old_subrange->range->closure != NULL) {
		old_subrange->range->closure->refcount--;
	}
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

value_p deref_subrange_p(subrange_p subrng) {
	if (subrng == NULL) {
		fprintf(stderr, "Exiting - asked to dereference a NULL pointer.\n");
		exit(-1);
	}
	if (subrng->subrange_num_rows == 1 && subrng->subrange_num_cols == 1) {
		return getVal(subrng->range, subrng->base_var_offset_row, subrng->base_var_offset_col);
	} else {
		value_p new_value = (value_p) malloc (sizeof(struct value_t));
		new_value->flags = FLAG_SUBRANGE;
		new_value->numericVal = 0.0;
		new_value->str = NULL;
		new_value->subrange = (subrange_p) malloc (sizeof(struct subrange_t));
		memcpy(new_value->subrange, subrng, sizeof(struct subrange_t));
		if (new_value->subrange->range->closure != NULL) {
			new_value->subrange->range->closure->refcount++;
		}
		return new_value;
	}
}

value_p new_subrange(int num_rows, int num_cols, value_p *vals) {
	/* This function does not check its arguments; if you supply fewer
	 * than num_rows * num_cols elements in vals, it will crash.
	 * Only use this function if you know what you're doing. */
	 struct subrange_t sr;
	 sr.range = (struct var_instance *) malloc (sizeof(struct var_instance));
	 sr.base_var_offset_row = 0;
	 sr.base_var_offset_col = 0;
	 sr.subrange_num_rows = num_rows;
	 sr.subrange_num_cols = num_cols;
	 sr.range->rows = num_rows;
	 sr.range->cols = num_cols;
	 sr.range->numFormulas = 0;
	 sr.range->formulas = NULL;
	 sr.range->closure = NULL;
	 sr.range->values = (value_p *) malloc(num_rows * num_cols * sizeof(value_p));
	 sr.range->status = (char *) malloc (num_rows * num_cols * sizeof(char));
	 sr.range->name = NULL;
	 int i;
	 for (i = 0; i < num_rows * num_cols; i++) {
		 sr.range->values[i] = clone_value(vals[i]);
		 sr.range->status[i] = CALCULATED;
	 }
	 return deref_subrange_p(&sr);
}

value_p box_command_line_args(int argc, char **argv) {
	value_p *vals = (value_p *) malloc (argc * sizeof(value_p));
	int i;
	for (i = 0; i < argc; i++) {
		vals[i] = new_string(argv[i]);
	}
	value_p ret = new_subrange(1, argc, vals);
	for (i = 0; i < argc; i++) {
		free(vals[i]);
	}
	free(vals);
	return ret;
}

char resolve_rhs_index(struct rhs_index *index, int dimension_len, int dimension_cell_num, int *result_ptr) {
	if (index == NULL) {
		fprintf(stderr, "Exiting - asked to dereference a NULL index\n");
		exit(-1);
	}
	int i;
	switch(index->rhs_index_type) {
		case RHS_IDX_ABSOLUTE:
			if (!assertSingleNumber(index->val_of_expr)) return false;
			i = (int) lrint(index->val_of_expr->numericVal);
			if (i >= 0) {
				*result_ptr = i;
			} else {
				*result_ptr = i + dimension_len;
			}
			return true;
			break;
		case RHS_IDX_RELATIVE:
			if (!assertSingleNumber(index->val_of_expr)) return false;
			*result_ptr = dimension_cell_num + (int) lrint(index->val_of_expr->numericVal);
			return true;
			break;
		case RHS_IDX_DIM_START:
			*result_ptr = 0;
			return true;
			break;
		case RHS_IDX_DIM_END:
			*result_ptr = dimension_len;
			return true;
			break;
		default:
			fprintf(stderr, "Exiting - illegal index type\n");
			exit(-1);
			break;
	}
}

char resolve_rhs_slice(struct rhs_slice *slice, int dimension_len, int dimension_cell_num, int *start_ptr, int *end_ptr) {
	char start_success, end_success;
	if (slice == NULL) {
		fprintf(stderr, "Exiting - asked to dereference a NULL slice\n");
		exit(-1);
	}
	if (slice->slice_start_index == NULL) {
		if (slice->slice_end_index != NULL) {
			fprintf(stderr, "Exiting - illegal slice\n");
			exit(-1);
		}
		if (dimension_len == 1) {
			*start_ptr = 0;
			*end_ptr = 1;
			return true;
		} else {
			*start_ptr = dimension_cell_num;
			*end_ptr = dimension_cell_num + 1;
			return true;
		}
	} else {
		start_success = resolve_rhs_index(slice->slice_start_index, dimension_len, dimension_cell_num, start_ptr);
		if (!start_success) return false;
		if (slice->slice_end_index == NULL) {
			*end_ptr = *start_ptr + 1;
			return true;
		} else {
			end_success = resolve_rhs_index(slice->slice_end_index, dimension_len, dimension_cell_num, end_ptr);
			return end_success;
		}
	}
}

value_p extract_selection(value_p expr, struct rhs_selection *sel, int r, int c) {
	int expr_rows, expr_cols;
	struct subrange_t subrange;
	struct rhs_slice *row_slice_p, *col_slice_p;
	int row_start, row_end, col_start, col_end;
	char row_slice_success, col_slice_success;

	if (expr == NULL || sel == NULL) {
		fprintf(stderr, "Exiting - asked to extract a selection using a NULL pointer.\n");
		exit(-1);
	}
	switch(expr->flags) {
		case FLAG_EMPTY:
			return new_val();
			break;
		case FLAG_NUMBER: case FLAG_STRING:
			expr_rows = 1;
			expr_cols = 1;
			break;
		case FLAG_SUBRANGE:
			expr_rows = expr->subrange->subrange_num_rows;
			expr_cols = expr->subrange->subrange_num_cols;
			break;
		default:
			fprintf(stderr, "Exiting - invalid value type\n");
			exit(-1);
			break;
	}
	if (sel->slice1 == NULL) {
		if (sel->slice2 != NULL) {
			fprintf(stderr, "Exiting - illegal selection\n");
			exit(-1);
		}
		row_slice_p = &corresponding_cell;
		col_slice_p = &corresponding_cell;
	} else {
		if (sel->slice2 == NULL) {
			if (expr_rows == 1) {
				row_slice_p = &zero_to_one;
				col_slice_p = sel->slice1;
			} else if (expr_cols == 1) {
				row_slice_p = sel->slice1;
				col_slice_p = &zero_to_one;
			} else {
				return new_val();
/*			Alternately:
				fprintf(stderr, "Runtime error: Only given one slice for a value with multiple rows and multiple columns\n");
				debug_print(expr);
				exit(-1); */
			}
		} else {
			row_slice_p = sel->slice1;
			col_slice_p = sel->slice2;
		}
	}
	row_slice_success = resolve_rhs_slice(row_slice_p, expr_rows, r, &row_start, &row_end);
	col_slice_success = resolve_rhs_slice(col_slice_p, expr_cols, c, &col_start, &col_end);
	if (!row_slice_success || !col_slice_success) return new_val();
	if (row_start < 0) row_start = 0;
	if (col_start < 0) col_start = 0;
	if (row_end > expr_rows) row_end = expr_rows;
	if (col_end > expr_cols)  col_end = expr_cols;
	if (row_end <= row_start || col_end <= col_start) return new_val();
	if (expr->flags == FLAG_NUMBER || expr->flags == FLAG_STRING) {
		/* You would have thought we could figure this out a lot further up
		 * in the code, but had to be sure that (row_start, row_end, col_start, col_end)
		 * actually ended up as (0, 1, 0, 1) */
		return clone_value(expr);
	} else {
		subrange.range = expr->subrange->range;
		subrange.base_var_offset_row = expr->subrange->base_var_offset_row + row_start;
		subrange.base_var_offset_col = expr->subrange->base_var_offset_col + col_start;
		subrange.subrange_num_rows = row_end - row_start;
		subrange.subrange_num_cols = col_end - col_start;
		return deref_subrange_p(&subrange);
	}
}

value_p getValSR(struct subrange_t *sr, int r, int c) {
	if(sr->subrange_num_rows <= r || sr->subrange_num_cols <= c || r < 0 || c < 0)
		return new_val();
	return getVal(sr->range, r + sr->base_var_offset_row, c + sr->base_var_offset_col);
}

void verify_assert(value_p val, char *fname) {
	if ((!assertSingleNumber(val)) || val->numericVal != 1.0) {
		fprintf(stderr, "EXITING - The function %s was called with arguments of the wrong dimensions.\n", fname);
		exit(-1);
	}
}

value_p getVal(struct var_instance *inst, int r, int c) {
	/* If we're going to return new_val() then we have to
	 * do clone_value(). Otherwise the receiver won't know
	 * whether or not they can free the value_p they get back.
	 * I think this should return, dangerously, return NULL if it's
	 * invalid, and the callers will have to be careful to check the value.
	 * The alternative is to always clone_value - safer, but much slower
	 * and makes our memory issues even bigger.
	 * Right now there are only a few places that call this. */

	if(!assertInBounds(inst, r, c)) return NULL;
	int cell_number = r * inst->cols + c;
	char cell_status = inst->status[cell_number];
	switch(cell_status) {
		case NEVER_EXAMINED:
			inst->status[cell_number] = IN_PROGRESS;
			inst->values[cell_number] = calcVal(inst, r, c);
			if (inst->values[cell_number]->flags == FLAG_SUBRANGE) {
				int i, j;
				for (i = 0; i < inst->values[cell_number]->subrange->subrange_num_rows; i++) {
					for (j = 0; j < inst->values[cell_number]->subrange->subrange_num_cols; j++) {
						/* Prevent sneaky circular references */
						getVal(inst->values[cell_number]->subrange->range,
									 i + inst->values[cell_number]->subrange->base_var_offset_row,
								   j + inst->values[cell_number]->subrange->base_var_offset_col);
					}
				}
			}
			inst->status[cell_number] = CALCULATED;
			break;
		case IN_PROGRESS:
			fprintf(stderr, "EXITING - Circular reference in %s[%d,%d]\n", inst->name, r, c);
			exit(-1);
			break;
		case CALCULATED:
			if (inst->values[cell_number] == NULL) {
				fprintf(stderr, "Supposedly, %s[%d,%d] was already calculated, but there is a null pointer there.\n", inst->name, r, c);
				fprintf(stderr, "Attempting to print contents of the variable instance where this occurred:\n");
				fflush(stderr);
				debug_print_varinst(inst);
				exit(-1);
			}
			break;
		default:
			fprintf(stderr, "Unrecognized cell status %d (row %d, col %d)!\n", cell_status, r, c);
			fprintf(stderr, "Attempting to print contents of the variable instance where this occurred:\n");
			fflush(stderr);
			debug_print_varinst(inst);
			exit(-1);
			break;
	}
	return inst->values[cell_number];
}
