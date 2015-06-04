#include <iostream>
#include <fstream>
#include <cstdlib>
#include <time.h>
#include "grid.h"

using namespace std;

static void help();
static void read_cli(int argc, char* argv[],string &output);
static void write_header(ofstream &output);
static void write_footer(ofstream &output);
static void write_initial(ofstream &output);
static void write_goal(ofstream &output);

static void generate(Grid *g,int n);

string program;

int main(int argc, char* argv[]) {
  string outputName;
  ofstream output;
  read_cli(argc,argv,outputName);
  
  output.open (outputName.c_str());
  
  write_header(output);

  /*****/
  // initial config generation
  
  
  /******/
  // goal config generation
  
  write_initial(output);
  write_goal(output);
  
  write_footer(output);
  output.close();
  return 0;
}

static void read_cli(int argc, char* argv[],string &output) {
  program = argv[0];
 
  argc--;
  argv++;
  if (argc == 0) {
    help();
  }

  while ((argc > 0) && (argv[0][0] == '-')) {
    switch(argv[0][1]) {
    case 'o':{
      if (argc < 1) { 
	help();
      }
      output = string(argv[1]);
      argc--;
      argv++;
    }
      break;
    case 'h':{
      help();
    }
      break;
    default:
      help();
    }
    argc--;
    argv++;
  }
}

static void write_header(ofstream &output) {
  output << "<?xml version=\"1.0\" standalone=\"no\" ?>\n";
  output << "<world gridSize=\"30,30\">\n";
  output << "\t<camera target=\"17,1,0\" directionSpherical=\"5,25,25\" angle=\"45\"/>\n";
  output << "\t<spotlight target=\"20,18,0\" directionSpherical=\"-45,40,50\" angle=\"30\"/>\n";
  
}

static void write_footer(ofstream &output) {
  output << "</world>\n";
}

static void write_initial(ofstream &output) {
  output << "\t<blockList color=\"100,100,100\" blocksize=\"1,5,1\">\n";
  output << "\t\t<block position=\"3,0\"/>\n";
  output << "\t</blockList>\n";
}

static void write_goal(ofstream &output) {
  output << "\t<targetGrid>\n";
  output << "\t\t<block position=\"4,0\"/>\n";		
  output << "\t</targetGrid>\n";
}

static void help() {
  cerr << program << " options:" << endl;
   cerr << "\t -o <name>\toutput xml file" << endl; 
   cerr << "\t -h \t\thelp" << endl;
   exit(EXIT_SUCCESS);
}

static void generate(Grid *g,int n) {
  int i = 0;
  int j = 0;
  
  srand (time(NULL));
  /*while(n > 0) {
    int j = rand();
    int k = rand();
    k = k % n; // k not bigger than the number of modules we want
    k = k % rect.x;
    n--;
    }*/
}
