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
	if (!scope_ptr->defns[varnum].isOneByOne) {
		fprintf(stderr, "The variable you claimed (%s) was one by one is not defined that way.\n", scope_ptr->defns[varnum].name);
	}
	struct var_instance *inst = get_variable(scope_ptr, varnum);
	if (inst->rows != 1 || inst->cols != 1) {
		fprintf(stderr, "The variable you claimed (%s) was one by one is actually %d by %d.\n", inst->name, inst->rows, inst->cols);
		debug_print_varinst(inst);
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
	memset(inst->values, 0, sizeof(value_p) * size);
	inst->status = malloc(sizeof(char) * size);
	memset(inst->status, 0, sizeof(char) * size);
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
			}
			if (def.formulas[i].isSingleRow) {
				inst->formulas[i].rowEnd = inst->formulas[i].rowStart + 1;
			} else if (def.formulas[i].toLastRow) {
				inst->formulas[i].rowEnd = inst->rows;
			} else {
				inst->formulas[i].rowEnd = getIntFromOneByOne(scope_ptr, def.formulas[i].rowEnd_varnum);
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

/* void setRange(value_p val, struct var_instance *inst) {
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
	setNumeric(res, 1); //
	return res;
} */

/* value_p deepCopy(value_p value) {
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
				// TODO: eval lazzzy
				*(v->values + offset) = getVal(value->subrange->range, i + value->subrange->offsetRow, j + value->subrange->offsetCol);
			}
		}
		setRange(_new, v);
	}
	return _new;
} */

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
		new_value->subrange->range->closure->refcount++;
		return new_value;
	}
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
	if(sr->base_var_offset_row + sr->subrange_num_rows <= r
		|| sr->base_var_offset_col + sr->subrange_num_cols <= c)
		return new_val();
	return getVal(sr->range, r + sr->base_var_offset_row, c + sr->base_var_offset_col);
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
// 	char *status = inst->status + offset;
// 	value_p return_val;
// 	if(*status & IN_PROGRESS) {
// 		/* TODO: Circular dependency. Possibly throw? */
// 		return_val = new_val();
// 	} else if ((~(*status)) & CALCULATED) { /* value not calculated */
// 		value_p val = calcVal(inst, x, y);
// 		inst->values[offset] = val;
// 		*status = (*status && !IN_PROGRESS) | CALCULATED;
// 		return_val = val;
// 	} else {
// 		return_val = inst->values[offset];
// 	}
// 	while(return_val->flags == FLAG_SUBRANGE && return_val->subrange->subrangeRow == 1 && return_val->subrange->subrangeCol == 1) {
// 		return_val = getVal(return_val->subrange->range, return_val->subrange->offsetRow, return_val->subrange->offsetCol);
// 	}
// //	debug_print_varinst(inst);
// 	return return_val;
}
