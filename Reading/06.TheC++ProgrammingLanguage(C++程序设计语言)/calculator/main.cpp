#include <iostream>
#include <string>
#include <map>
#include <cctype>
#include <sstream>
#include "calculator.h"
using namespace std;

istream* input;

int main(int argc, char* argv[])
{
  switch(argc){
  case 1:
    input = &cin;
    break;
  case 2:
    input = new istringstream(argv[1]);
    break;
  default:
    error("too many arguments");
    return 1;
  }


  table["pi"] = 3.14259;
  table["e"] = 2.71828;

  while(*input){
    get_token();
    if(curr_tok == END) break;
    if(curr_tok == PRINT) continue;
    cout << expr(false) << '\n';
  }

  if(input != &cin) delete input;

  return no_of_errors;
}
