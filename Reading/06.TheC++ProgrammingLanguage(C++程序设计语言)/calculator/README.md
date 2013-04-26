# calculator

第六章是通过设计一个计算器来介绍C++的表达式和语句的，由于书中以设计编译器的方法来设计这个计算器，因此觉得有点意思，也是自己熟悉C++的过程。

这个计算器（编译器）以4个部分组成：
* 分析器：语法分析工作
* 输入函数：处理输入和词法分析工作
* 符号表：保留持久性信息
* 驱动程序：处理初始化、输出和错误

# 分析器

该计算器所接受的语言语法：
```cpp
program:
    END                         //END表示输入结束
	expr_list END
expr_list:
    expression PRINT            //PRINT是分号
	expression PRINT expr_list
expression:
    expression + term
	expression - term
	term
term:
    term / primary
	term * primary
	primary
primary:
    NUMBER
	NAME
	NAME = expression
	- primary
	(expression)
```

终结符（例如END、NUMBER、+和-）由词法分析程序get_token()识别，而非终结符则由语法分析程序expr(),term(),prim()识别。一旦一个（子）表达式的两个运算对象已经知道，立即对这个表达式求值；对于编译器，就是生成代码。

每个分析函数都有一个bool参数，指明该函数是否需要调用get_token()去取得下一个单词。

* expr()函数处理加和减；
* term()函数处理乘与除；
* prim()函数处理语法；
* error()函数处理错误；
* get_token()分析输入；
* main()驱动整个程序处理;

```cpp
#include <iostream>
#include <string>
#include <map>
#include <cctype>
using namespace std;

//double ture = 0;
//double flase = 1;





enum Token_value {
  NAME,        NUMBER,      END,
  PLUS='+',    MINUS='-',   MUL='*',    DIV='/',
  PRINT=';',   ASSIGN='=',  LP='(',     RP=')'
};

Token_value curr_tok = PRINT;
map<string,double> table;

Token_value get_token();
double prim(bool);
double term(bool);

int no_of_errors;

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

double number_value;
string string_value;

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
Token_value get_token()
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

int  main()
{
  table["pi"] = 3.1415926;
  table["e"] = 2.71828;

  while(cin){
    get_token();
    if(curr_tok == END) break;
    if(curr_tok == PRINT) continue;
    cout << expr(false) << '\n';
  }

  return no_of_errors;
}
```


