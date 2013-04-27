#include <string>
#include <map>
using namespace std;

enum Token_value {
  NAME,
  NUMBER,
  END,
  PLUS='+',
  MINUS='-',
  MUL='*',
  DIV='/',
  PRINT=';',
  ASSIGN='=',
  LP='(',
  RP=')'
};

extern Token_value curr_tok;
extern map<string,double> table;
extern int no_of_errors;
extern istream* input;
extern double number_value;
extern string string_value;


extern Token_value get_token();
extern double prim(bool);
extern double term(bool);
extern double error(const string& s);
extern double expr(bool get);
