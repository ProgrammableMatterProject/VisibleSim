#ifndef ARGUMENTS_H_
#define ARGUMENTS_H_

#include <string>

class Arguments {
public:
  std::string input;
  std::string output;

  Arguments();
  Arguments(int argc, char *argv[]);
  Arguments(const Arguments &a);
  ~Arguments();

  void help();
  
};

#endif
