
/* Value type */
#define FLAG_EMPTY 0
#define FLAG_NUMBER 1
#define FLAG_STRING 2
#define FLAG_SUBRANGE 3

/* Status flag */
#define CALCULATED 2
#define IN_PROGRESS 4

#define FLOAT_CUTOFF 1e-7


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

struct ExtendScope;
typedef value_p (*FormulaFP) (struct ExtendScope *scope, int row, int col);

struct ExtendFormula {
  /* These 10 variables correspond to formula_row_start through formula_col_end,
   * where bool singleRow/Col are true if formula_row_end is None */
  bool fromFirstRow;
  int rowStart_varnum;
  bool toLastRow;
  int rowEnd_varnum;
  bool fromFirstCol;
  int colStart_varnum;
  bool toLastCol;
  int colEnd_varnum;

  FormulaFP formula;
};

struct ResolvedFormula {
	int rowStart, rowEnd, colStart, colEnd;
	FormulaFP formula;
};

struct var_defn {
  /* This is like a class definition - for every declared variable in the
   * Extend source, there should be one instance of these per compiled program.
   * They should just live in the global program storage.
   * It corresponds to Ast.variable */
   int rows_varnum;
   int cols_varnum;
   int numFormulas;
   struct ExtendFormula *formulas;
	 bool isOneByOne;
	 char *name;
};
struct var_instance {
  /* This is an actual instance of a variable - we get one of these
   * per variable per time a function is called (assuming the contents
   * of the variable get examined.  */
	int rows, cols;
	int numFormulas;
	struct ResolvedFormula *formulas;
  struct ExtendScope *closure;
  value_p *values;
	char *status;
	char *name;
};
struct ExtendScope {
  struct var_defn *defns;
  struct var_instance **vars;
	int numVars;
	int refcount;
	value_p *functionParams;
};

struct subrange_t {
	struct var_instance *range;
	int offsetRow;
	int offsetCol;
	int subrangeRow;
	int subrangeCol;
};

typedef struct subrange_t* subrange_p;


string_p new_string(char *str);
value_p box_value_string(string_p);
value_p getVal(struct var_instance *inst, int x, int y);
double setNumeric(value_p result, double val);
char* setString(value_p result, char *str, int length);
double setFlag(value_p result, double flag_num);
int assertSingle(value_p value);
int assertSingleNumber(value_p p);
int assertText(value_p my_val);
int assertSingleString(value_p p);
int assertEmpty(value_p p);
value_p new_val();
value_p new_number(double val);
struct var_instance *get_variable(struct ExtendScope *scope_ptr, int varnum);
void null_init(struct ExtendScope *scope_ptr);
struct var_instance *instantiate_variable(struct ExtendScope *scope_ptr, struct var_defn def);
struct var_instance *get_variable(struct ExtendScope *scope_ptr, int varnum);
bool assertInBounds(struct var_instance *defn, int x, int y);
bool fitsDim(int dim, int rowStart_varnum, int rowEnd_varnum);
bool fitsRange(struct ResolvedFormula *formula, int x, int y);
value_p calcVal(struct var_instance *inst, int x, int y);
void setRange(value_p val, struct var_instance *inst);
value_p getSize(struct var_instance *inst);
value_p deepCopy(value_p value);
value_p clone_value(value_p old_value);
void delete_string_p(string_p old_string);
void delete_subrange_p(subrange_p old_subrange);
void delete_value(value_p old_value);
value_p getVal(struct var_instance *inst, int x, int y);
