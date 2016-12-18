#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<stdbool.h>
#include "../../lib/gdchart0.94b/gdc.h"
#include "../../lib/gdchart0.94b/gdchart.h"
/* #include <sys/time.h> */
#include <time.h>
#include "runtime.h"

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

value_p extend_print(value_p whatever, value_p text) {
	if(!assertSingleString(text)) return new_val();
	if(!assertText(text)) return new_val();
	printf("%s", text->str->text);
	return new_val();
}

value_p extend_printv(value_p whatever, value_p text) {
	printf("%s", text->str->text);
	return new_val();
}

value_p extend_printd(value_p whatever, value_p text) {
	printf("%f\n", text->numericVal);
	value_p result = malloc(sizeof(struct value_t));
	return result;
}

value_p extend_to_string(value_p val) {
		if(assertSingleNumber(val)) {
			double possible_num = val->numericVal;
			int rounded_int = (int) lrint(possible_num);
			char *converted_str;
			if (fabs(possible_num - rounded_int) < FLOAT_CUTOFF) {
				int size = snprintf(NULL, 0, "%d", rounded_int);
				converted_str = malloc(size + 1);
				sprintf(converted_str, "%d", rounded_int);
			} else {
				int size = snprintf(NULL, 0, "%f", possible_num);
				converted_str = malloc(size + 1);
				sprintf(converted_str, "%f", possible_num);
			}
			value_p result = new_string(converted_str);
			free(converted_str);
			return result;
		}
		else if(assertSingleString(val)) return val;
		else if(val->flags == FLAG_EMPTY) {
 			return new_string("empty");
 		}
		else if(val->flags == FLAG_SUBRANGE) {
			int i,j,len;
			value_p value;
			char *result, *res;
			len = 0;
			subrange_p sr = val->subrange;
			value_p *strs = malloc(sizeof(value_p) * sr->subrange_num_cols * sr->subrange_num_rows);
			for(i = 0; i < sr->subrange_num_rows; i++) {
				for(j = 0; j < sr->subrange_num_cols; j++) {
					value = extend_to_string(getValSR(sr, i, j));
					//debug_print(value, "");
					strs[i * sr->subrange_num_cols + j] = value;
					len += value->str->length;
				}
			}
			len += sr->subrange_num_rows * sr->subrange_num_cols + 1 /*closing paren*/;
			res = result = malloc(len + 1/*terminal character*/);
			*result = '{';
			result++;
			for(i = 0; i < sr->subrange_num_rows; i++) {
				for(j = 0; j < sr->subrange_num_cols; j++) {
					memcpy(result,strs[i * sr->subrange_num_cols + j]->str->text, strs[i * sr->subrange_num_cols + j]->str->length);
					result += strs[i * sr->subrange_num_cols + j]->str->length;
					if(j != sr->subrange_num_cols - 1) {
						*result = ',';
						result++;
					}
				}
				if(i != sr->subrange_num_rows - 1) {
					*result = ';';
					result++;
				}
			}
			*result = '}';
			value_p v = new_string(res);
			free(res);
			return v;
		} else {
			__builtin_unreachable();
		}
		// If the struct does not hold a string or number, return empty?
		return new_val();
}

#define EXPOSE_MATH_FUNC(name) value_p extend_##name(value_p a){if(!assertSingleNumber(a)) return new_val();double val = name(a->numericVal);return new_number(val);}
EXPOSE_MATH_FUNC(sin)
EXPOSE_MATH_FUNC(cos)
EXPOSE_MATH_FUNC(tan)
EXPOSE_MATH_FUNC(acos)
EXPOSE_MATH_FUNC(asin)
EXPOSE_MATH_FUNC(atan)
EXPOSE_MATH_FUNC(sinh)
EXPOSE_MATH_FUNC(cosh)
EXPOSE_MATH_FUNC(tanh)
EXPOSE_MATH_FUNC(exp)
EXPOSE_MATH_FUNC(log)
EXPOSE_MATH_FUNC(log10)
EXPOSE_MATH_FUNC(sqrt)
EXPOSE_MATH_FUNC(ceil)
EXPOSE_MATH_FUNC(fabs)
EXPOSE_MATH_FUNC(floor)

value_p extend_round(value_p num, value_p number_of_digits) {
	if (!assertSingleNumber(num) || !assertSingleNumber(number_of_digits)) return new_val();
	double factor_of_10 = pow(10,number_of_digits->numericVal);
	return new_number(rint(num->numericVal * factor_of_10) / factor_of_10);
}

value_p extend_len(value_p str_val) {
	if (!assertSingleString(str_val)) return new_val();
	return new_number((double) str_val->str->length);
}

value_p extend_get_stdin() {
	if (open_num_files + 1 > MAX_FILES) {
		return new_val();
	} else {
		open_num_files++;
		open_files[open_num_files] = stdin;
		return new_number((double) open_num_files);
	}
}

value_p extend_get_stdout() {
	if (open_num_files + 1 > MAX_FILES) {
		return new_val();
	} else {
		open_num_files++;
		open_files[open_num_files] = stdout;
		return new_number((double) open_num_files);
	}
}

value_p extend_get_stderr() {
	if (open_num_files + 1 > MAX_FILES) {
		return new_val();
	} else {
		open_num_files++;
		open_files[open_num_files] = stderr;
		return new_number((double) open_num_files);
	}
}

value_p extend_open(value_p filename, value_p mode){
	FILE *val;
	if (   !assertSingleString(filename)
			|| !assertSingleString(mode)
			|| open_num_files + 1 > MAX_FILES) {
				return new_val();
	}
	val = fopen(filename->str->text, mode->str->text);
	if(val == NULL) return new_val();
	open_num_files++;
	open_files[open_num_files] = val;
	return new_number((double) open_num_files);
}

value_p extend_close(value_p file_handle) {
	if(!assertSingleNumber(file_handle)) {
		// Per the LRM this is actually supposed to crash the program.
		fprintf(stderr, "EXITING - Attempted to close something that was not a valid file pointer\n");
		exit(-1);
	}
	int fileNum = (int) file_handle->numericVal;

	if (fileNum > open_num_files || open_files[fileNum] == NULL) {
		// Per the LRM this is actually supposed to crash the program.
		fprintf(stderr, "EXITING - Attempted to close something that was not a valid file pointer\n");
		exit(-1);
	}
	fclose(open_files[fileNum]);
	open_files[fileNum] = NULL; // Empty the container for the pointer.
	return new_val(); // asssuming it was an open valid handle, close() is just supposed to return empty
}

value_p extend_read(value_p file_handle, value_p num_bytes){
	/* TODO: Make it accept empty */
	if(!assertSingleNumber(file_handle) || !assertSingleNumber(num_bytes)) return new_val();
	int max_bytes = (int)num_bytes->numericVal;
	int fileNum = (int)file_handle->numericVal;
	if (fileNum > open_num_files || open_files[fileNum] == NULL)  return new_val();
	FILE *f = open_files[fileNum];
	max_bytes = (int) num_bytes->numericVal;
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
	value_p result = new_string(buf);
	free(buf);
	return result;
	//edge case: how to return the entire contents of the file if n == empty?
}

value_p extend_readline(value_p file_handle) {
	int	i=0, buf_size = 256;
	char next_char;
	if (!assertSingleNumber(file_handle)) return new_val();
	int fileNum = (int) file_handle->numericVal;
	FILE *f = open_files[fileNum];
	if (fileNum > open_num_files || open_files[fileNum] == NULL) {
		return new_val();
	}
	char *buf = (char *) malloc (buf_size * sizeof(char));
	while ((next_char = fgetc(f)) != '\n') {
		buf[i++] = next_char;
		if (i == buf_size - 2) {
			buf_size *= 2;
			char *new_buf = (char *) malloc (buf_size * sizeof(char));
			memcpy(new_buf, buf, i);
			free(buf);
			buf = new_buf;
		}
	}
	buf[i] = '\0';
	value_p result = new_string(buf);
	free(buf);
	return result;
}

value_p extend_write(value_p file_handle, value_p buffer){
	if(!assertSingleNumber(file_handle) || !assertSingleString(buffer)) return new_val();
	int fileNum = (int) file_handle->numericVal;
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

value_p extend_plot(value_p file_name){
	// extract the numerical values from the first parameter - values
	if(!assertSingle(file_name)) return new_val();
	float a[6]  = { 0.5, 0.09, 0.6, 0.85, 0.0, 0.90 },
				b[6]  = { 1.9, 1.3,  0.6, 0.75, 0.1, 2.0 };
	char *t[6] = { "Chicago", "New York", "L.A.", "Atlanta", "Paris, MD\n(USA) ", "London" };
	unsigned long sc[2]    = { 0xFF8080, 0x8080FF };
	GDC_BGColor   = 0xFFFFFFL;
	GDC_LineColor = 0x000000L;
	GDC_SetColor  = &(sc[0]);
	GDC_stack_type = GDC_STACK_BESIDE;
	// Using the line below, can also spit to stdout and fwrite from Extend
	// printf( "Content-Type: image/png\n\n" );
	FILE *outpng = fopen("extend.png", "wb");
	out_graph(250, 200, outpng, GDC_3DBAR, 6, t, 2, a, b);
	fclose(outpng);
	return new_val();
}

value_p extend_bar_chart(value_p file_handle, value_p labels, value_p values){
	// Mandates 1 row, X columns
	if(!assertSingleNumber(file_handle)) return new_val();
	int fileNum = (int)file_handle->numericVal;
	if (fileNum > open_num_files || open_files[fileNum] == NULL)  return new_val();
	FILE *f = open_files[fileNum];
	int data_length = labels->subrange->subrange_num_cols;
	if(data_length != values->subrange->subrange_num_cols) return new_val();

	float *graph_values = malloc(sizeof(float) * data_length);
	char **graph_labels = malloc(sizeof(char*) * data_length);
	for(int i = 0; i < data_length; i++){
		graph_labels[i] = getValSR(labels->subrange, 0, i)->str->text;
		graph_values[i] = (float)getValSR(values->subrange, 0, i)->numericVal;
	}
	unsigned long sc[2] = {0xFF8080, 0x8080FF};
	GDC_BGColor   = 0xFFFFFFL;
	GDC_LineColor = 0x000000L;
	GDC_SetColor  = &(sc[0]);
	GDC_stack_type = GDC_STACK_BESIDE;
	out_graph(250, 200, f, GDC_3DBAR, data_length, graph_labels, 1, graph_values);
	// width, height, file handle, graph type, number of data points, labels, number of data sets, the data sets
	free(graph_labels);
	free(graph_values);
	fclose(f);
	return new_val();
}

value_p extend_line_chart(value_p file_handle, value_p labels, value_p x_values){
	if(!assertSingleNumber(file_handle)) return new_val();
	int fileNum = (int)file_handle->numericVal;
	if (fileNum > open_num_files || open_files[fileNum] == NULL)  return new_val();
	FILE *f = open_files[fileNum];
	int data_length = labels->subrange->subrange_num_cols;
	if(data_length != x_values->subrange->subrange_num_cols) return new_val();
	float *graph_x_values = malloc(sizeof(float) * data_length);
	char **graph_labels = malloc(sizeof(char*) * data_length);
	for(int i = 0; i < data_length; i++){
		graph_labels[i] = getValSR(labels->subrange, 0, i)->str->text;
		graph_x_values[i] = (float)getValSR(x_values->subrange, 0, i)->numericVal;
	}
	unsigned long sc[2] = {0xFF8080, 0x8080FF};
	GDC_BGColor   = 0xFFFFFFL;
	GDC_LineColor = 0x000000L;
	GDC_SetColor  = &(sc[0]);
	GDC_stack_type = GDC_STACK_BESIDE;
	out_graph(250, 200, f, GDC_LINE, data_length, graph_labels, 1, graph_x_values);
	free(graph_labels);
	free(graph_x_values);
	fclose(f);
	return new_val();
}

value_p extend_current_hour() {
	time_t ltime;
	struct tm info;
	ltime = time(&ltime);
	localtime_r(&ltime, &info);
	return new_number((double) info.tm_hour);
}

value_p extend_isNaN(value_p val) {
	if (!assertSingleNumber(val)) return new_val();
	double d = val->numericVal;
	return isnan(d) ? new_number(1.0) : new_number(0.0);
}

value_p extend_isInfinite(value_p val) {
	if (!assertSingleNumber(val)) return new_val();
	double d = val->numericVal;
	if (isinf(d)) {
			return d < 0 ? new_number(-1.0) : new_number(1.0);
	} else {
		return new_number(0.0);
	}
}

value_p extend_toASCII(value_p val) {
	if (!assertSingleString(val)) return new_val();
	value_p *val_arr = malloc(sizeof(value_p) * val->str->length);
	int i;
	for(i = 0; i < val->str->length; i++) {
		value_p my_val = malloc(sizeof(struct value_t));
		my_val->flags = FLAG_NUMBER;
		my_val->numericVal = (double)val->str->text[i];
		val_arr[i] = my_val;
	}
	value_p _new = new_subrange(1,val->str->length, val_arr);
	return _new;
}

value_p extend_fromASCII(value_p val) {
	if(val->flags == FLAG_NUMBER) {
		char s[2];
		s[0] = ((char)lrint(val->numericVal));
		s[1] = '\0';
		return new_string(s);
	}
	else if(val->flags == FLAG_SUBRANGE) {
		int rows, cols, len;
		rows = val->subrange->subrange_num_rows;
		cols = val->subrange->subrange_num_cols;
		if(rows > 1 && cols > 1) return new_val();
		else len = (rows == 1 ? cols : rows);
		char *text = malloc(1 + sizeof(char) * len);
		for(rows = 0; rows < val->subrange->subrange_num_rows; rows++) {
			for(cols = 0; cols < val->subrange->subrange_num_cols; cols++) {
				value_p single = getValSR(val->subrange, rows, cols);
				if(single->flags != FLAG_NUMBER) {
					free(text);
					return new_val();
				}
				text[rows + cols] = (char)lrint(single->numericVal);
			}
		}
		text[len] = '\0';
		value_p ret = new_string(text);
		free(text);
		return ret;
	}
	return new_val();
}
