#include <cstdlib>
#include <iostream>
#include <fstream>

#include "arguments.hpp"
#include "targa.hpp"
#include "pixel.hpp"

using namespace std;

int main(int argc, char *argv[]) {

  
  Arguments arguments(argc,argv);
  cerr << "Input: " << arguments.input << endl;
  cerr << "Output: " << arguments.output << endl;

  cerr << endl;
  Targa tga(arguments.input);
  cerr << arguments.input << " opened!" << endl;
  
  ofstream output;
  output.open (arguments.output);
  tga.exportToVisibleSim(output);
  cerr << arguments.input << " exported in " << arguments.output << endl;
  return EXIT_SUCCESS;
}
