#include<stdio.h>
#include<stdlib.h>
#include<math.h>

struct subrange_t;
struct value_t;

struct number_t {
	int val;
};

struct formula_t {
	struct value_t (*formula)(struct value_t);
};

typedef struct formula_t* formula_p;

struct value_t {
	char flags;
	struct number_t numericVal;
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

value_p get_val(range_p range, int row, int col);

int assertSingle(subrange_p range) {
	return (range->subrangeRow == 1 && range->subrangeCol == 1);
}

value_p extend_sin(subrange_p range) {
	double val;
	value_p result = malloc(sizeof(struct value_t));
	result->flags = (char)0;
	if(!assertSingle(range)) return result;
	value_p initial = get_val(range->range, range->offsetRow, range->offsetCol);
	val = sin(initial->numericVal.val);
	result->numericVal.val = val;
	result->flags = (char)1;
	return result;
}
