#include<stdio.h>
#include<stdlib.h>
#include<math.h>

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
	int numericVal;
	string_p str;
	struct subrange_t *subrange;
	double doubleVal;
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

int assertSingle(subrange_p range) {
	return (range->subrangeRow == 1 && range->subrangeCol == 1);
}

int assertText(value_p my_val) {
	return (my_val->flags == 2);
}

value_p empty() {
	value_p empty_val = malloc(sizeof(struct value_t));
	empty_val->flags = 0;
	return empty_val;
}

value_p success() {
	value_p val = malloc(sizeof(struct value_t));
	val->flags = 1;
	val->numericVal = 1;
	return val;
}

value_p print(subrange_p whatever, subrange_p text) {
	if(!assertSingle(text)) return empty();
	value_p my_val = get_val(text,0,0);
	if(!assertText(my_val)) return empty();
	printf("%s", my_val->str->text);
	return success();
}

value_p printn(subrange_p whatever, subrange_p text) {
	printf("%d", text->range->values->numericVal);
	value_p result = malloc(sizeof(struct value_t));
	return result;
}

value_p printd(subrange_p whatever, subrange_p text) {
	printf("%f\n", text->range->values->doubleVal);
	value_p result = malloc(sizeof(struct value_t));
	return result;
}

/*
subrange_p _extend_abc() {
	printf("Fun2\n");
	subrange_p result = malloc(sizeof(struct subrange_t));
	return result;
}*/
value_p extend_sin(subrange_p range) {
	double val;
	if(!assertSingle(range)) return empty();
	value_p initial = get_val(range, 0, 0);
	val = sin(initial->doubleVal);
	value_p result = empty();
	result->doubleVal = val;
	result->flags = (char)4;
	return result;
}
