#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<stdbool.h>
/* #include <sys/time.h> */
#include <time.h>
#include "runtime.h"

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
			value_p result = box_value_string(new_string(converted_str));
			return result;
		}
		else if(assertSingleString(val)) return val;
		else if(val->flags == FLAG_EMPTY) {
 			value_p _new = new_val();
 			setString(_new, "empty", 5);
 			return _new;
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
			value_p ret_val = new_val();
			setString(ret_val, res, len);
			return ret_val;
		} else {
			__builtin_unreachable();
		}
		// If the struct does not hold a string or number, return empty?
		return new_val();
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
	if(!assertSingleNumber(file_handle) || !assertSingleNumber(num_bytes)) return new_val();
	int max_bytes;
	int fileNum = (int) file_handle->numericVal;
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
	value_p result = box_value_string(new_string(buf));
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
	value_p result = box_value_string(new_string(buf));
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

value_p extend_current_hour() {
	time_t ltime;
	struct tm info;
	ltime = time(&ltime);
	localtime_r(&ltime, &info);
	return new_number((double) info.tm_hour);
}
