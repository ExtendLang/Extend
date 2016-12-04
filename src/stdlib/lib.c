#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#define FLAG_EMPTY 0
#define FLAG_NUMBER 1
#define FLAG_STRING 2
#define FLAG_SUBRANGE 3

FILE *open_files[256];
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

value_p extend_open(subrange_p range_one, subrange_p range_two){
	FILE *val;
	if(!assertSingle(range_one)) return new_val();
	if(!assertSingle(range_two)) return new_val();
	if(open_num_files + 1 > 255) return new_val();
	value_p filename = get_val(range_one, 0, 0);
	value_p mode = get_val(range_two, 0,0);
	val = fopen(filename->str->text, mode->str->text);
	if(val == NULL) return new_val();
	open_num_files++;
	open_files[open_num_files] = val;
	value_p result = new_val();
	setNumeric(result, open_num_files);
	setFlag(result, FLAG_NUMBER);
	return result;
}

// test is currently not working - box_single_value subrange arg problem?
value_p extend_close(subrange_p range){
	double val;
	if(!assertSingle(range)) return new_val();
	value_p ind = get_val(range, 0, 0);
	val = fclose(open_files[(int)ind->numericVal]);
	open_files[(int)ind->numericVal] = NULL; // Empty the container for the pointer.
	value_p result = new_val();
	setNumeric(result, val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

// untested
value_p extend_read(subrange_p buf, subrange_p s, subrange_p n, subrange_p f){
	double val;
	if(!assertSingle(buf)) return new_val();
	if(!assertSingle(s)) return new_val();
	if(!assertSingle(n)) return new_val();
	if(!assertSingle(f)) return new_val();
	value_p buffer = get_val(buf, 0, 0);
	value_p size = get_val(s, 0, 0);
	value_p num = get_val(n, 0, 0);
	value_p fd = get_val(f, 0, 0);
	// is this supposed to be buffer->subrange? or text?
	val = fread(buffer->subrange, size->numericVal, num->numericVal, open_files[(int)fd->numericVal]);
	value_p result = new_val();
	setNumeric(result, val);
	setFlag(result, FLAG_NUMBER);
	return result;
}

// untested
value_p extend_write(subrange_p buf, subrange_p s, subrange_p n, subrange_p f){
	double val;
	if(!assertSingle(buf)) return new_val();
	if(!assertSingle(s)) return new_val();
	if(!assertSingle(n)) return new_val();
	if(!assertSingle(f)) return new_val();
	value_p buffer = get_val(buf, 0, 0);
	value_p size = get_val(s, 0, 0);
	value_p num = get_val(n, 0, 0);
	value_p fd = get_val(f, 0, 0);
	// is this supposed to be buffer->subrange? or text? Same q as above.
	val = fwrite(buffer->subrange, size->numericVal, num->numericVal, open_files[(int)fd->numericVal]);
	value_p result = new_val();
	setNumeric(result, val);
	setFlag(result, FLAG_NUMBER);
	return result;
}
