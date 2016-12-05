#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#define FLAG_EMPTY 0
#define FLAG_NUMBER 1
#define FLAG_STRING 2
#define FLAG_SUBRANGE 3

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

string_p new_string(char *str);

value_p box_value_string(string_p);

value_p get_val(subrange_p range, int row, int col) {
	//TODO: assertions
	value_p val = range->range->values + (row + range->offsetRow) * range->range->cols + col + range->offsetCol;
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
	printf("%f\n", text->range->values->numericVal);
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

value_p extend_read(subrange_p rng_file_handle, subrange_p rng_num_bytes){
	/* TODO: Make it accept empty */
	if(!assertSingleNumber(rng_file_handle) || !assertSingleNumber(rng_num_bytes)) return new_val();
	int max_bytes = (int) get_number(rng_num_bytes);
	int fileNum = (int) get_number(rng_file_handle);
	if (fileNum > open_num_files || open_files[fileNum] == NULL)  return new_val();
	char *buf = malloc(sizeof(char) * (max_bytes + 1));
	int bytes_read = fread(buf, sizeof(char), max_bytes, open_files[fileNum]);
	buf[bytes_read + 1] = 0;
	value_p result = box_value_string(new_string(buf));
	free(buf);
	return result;
	//edge case: how to return the entire contents of the file if n == empty?
}

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
