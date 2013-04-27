// calculator.cpp : A Simple Calculator From Book

// error(): print error messages
// get_token(): get token from input
// prim(): compire the input
// term(): div and mul
// expr(): expression  


#include <iostream>
#include <string>
#include <map>
#include <cctype>
#include <sstream>
#include "calculator.h"
using namespace std;

Token_value curr_tok = PRINT;
map<string,double> table;
int no_of_errors;
double number_value;
string string_value;


double error(const string& s)
{
  no_of_errors++;
  cerr << 'error:' << s << '\n'; // std::cerr,std::cin,std::cout
  return 1;
}


double expr(bool get)		// 加和减
{
  double left = term(get);

  for(;;)
    switch(curr_tok){
    case PLUS:
      left += term(true);
      break;
    case MINUS:
      left -= term(true);
      break;
    default:
      return left;
    }
}

double term(bool get)		// 乘和除
{
  double left = prim(get);

  for(;;)
    switch(curr_tok){
    case MUL:
      left *= prim(true);
      break;
    case DIV:
      if(double d = prim(true)){
	left /= d;
	break;
      }
      return error("divide by 0");
    default:
      return left;
    }
}

double prim(bool get)		// 处理初等项
{
  if(get) get_token();

  switch (curr_tok){
  case NUMBER:
    {
      double v = number_value;
      get_token();
      return v;
    }
  case NAME:			// NAME, NAME=, or error
    {
      double& v = table[string_value];
      if(get_token() == ASSIGN) v = expr(true);
      return v;
    }
  case MINUS:			// 一元
    return -prim(true);
  case LP:
    {
      double e = expr(true);
      if(curr_tok != RP) return error(") expected");
      get_token();
      return e;
    }
  default:
    return error("primary expected");
  }
}


// v1
Token_value get_token_old()
{
  char ch = 0;
  cin >> ch;
  
  switch(ch){
  case 0:
    return curr_tok = END;
  case ';':
  case '*':
  case '/':
  case '+':
  case '-':
  case '(':
  case ')':
  case '=':
    return curr_tok = Token_value(ch);
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case '.':
    cin.putback(ch);
    cin >> number_value;
    return curr_tok = NUMBER;
  default:
    if(isalpha(ch)){
      cin.putback(ch);
      cin >> string_value;
      return curr_tok = NAME;
    }
    error("bad token");
    return curr_tok = PRINT;
  }
}

// use *input ,not cin
Token_value get_token()
{
  char ch;

  do {				// 跳过空格，除了'\n'
    if(!input->get(ch)) return curr_tok = END;
  } while(ch != '\n' && isspace(ch));

  switch(ch){
  case ';':
  case '\n':
    return curr_tok = PRINT;
  case '*':
  case '/':
  case '+':
  case '-':
  case '(':
  case ')':
  case '=':
    return curr_tok = Token_value(ch);
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case '.':
    input->putback(ch);
    *input >> number_value;
    return curr_tok = NUMBER;
  default:
    if(isalpha(ch)){
      string_value = ch;
      while (input->get(ch) && isalnum(ch)) 
	string_value.push_back(ch);
      input->putback(ch);
      return curr_tok=NAME;
    }
    error("bad token");
    return curr_tok = PRINT;
  }
}
