#include <iostream>
#include <fstream>
#include <cstdlib>
#include <time.h>
#include "grid.h"
#include <cmath>
#include <climits>

using namespace std;

enum configuration_t {INITIAL = 0, GOAL = 1};

static void help();
static void read_cli(int argc, char* argv[],string &output, int &n,double &r);
string program;

static void write_header(ofstream &output);
static void write_footer(ofstream &output);
static void write_configuration(ofstream &output,Grid *g, Rectangle &a);
static void write_initial(ofstream &output,Grid *g);
static void write_goal(ofstream &output,Grid *g);

static Grid* initGrid(int n, double d);
static void generate(Grid *g,configuration_t c,int n);
static double euclideanDistance(Coordinate p1, Coordinate p2);
static bool areNeighbors(Coordinate p1, Coordinate p2);

int main(int argc, char* argv[]) {
  string outputName;
  int n;
  double ratio;
  ofstream output;
  read_cli(argc,argv,outputName,n,ratio);
  
  output.open (outputName.c_str());
  
  write_header(output);
  
  Grid * grid = initGrid(n,ratio);
  
  cout << "grid created" << endl;

  // initial config generation
  generate(grid,INITIAL,n);
  cout << "initial config generated" << endl;
  
  // goal config generation
  generate(grid,GOAL,n);
  cout << "goal config generated" << endl;

  write_initial(output,grid);
  write_goal(output,grid);
  cout << "configuration written in " << outputName << endl;

  write_footer(output);
  output.close();
  cout << outputName << " closed!" << endl;
  return 0;
}

static void read_cli(int argc, char* argv[],string &output,int &n,double &r) {
  program = argv[0];
 
  argc--;
  argv++;
  if (argc == 0) {
    help();
  }

  bool n_set = false;
  bool o_set = false;

  while ((argc > 0) && (argv[0][0] == '-')) {
    switch(argv[0][1]) {
    case 'o':{
      if (argc < 1) { 
	help();
      }
      output = string(argv[1]);
      o_set = true;
      argc--;
      argv++;
    }
      break;
    case 'n': {
      if (argc < 1) { 
	help();
      }
      n = atoi(argv[1]);
      n_set = true;
      argc--;
      argv++;
    }
      break; 
    case 'r': {
      if (argc < 1) { 
	help();
      }
      r = atof(argv[1]);
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

  if (!n_set || !o_set) {
    help();
  }
}

static void write_header(ofstream &output) {

  // 3* to allow modules to move to the top.
  output << "<?xml version=\"1.0\" standalone=\"no\" ?>\n";
  output << "<world gridSize=\"30,30\">\n";
  output << "\t<camera target=\"17,1,0\" directionSpherical=\"5,25,25\" angle=\"45\"/>\n";
  output << "\t<spotlight target=\"20,18,0\" directionSpherical=\"-45,40,50\" angle=\"30\"/>\n";
  
}

static void write_footer(ofstream &output) {
  output << "</world>\n";
}

static void write_configuration(ofstream &output,Grid *g, Rectangle &a) {
  Coordinate p = a.getPBottomLeft();
  
  for(int i = 0; i < a.getWidth(); i++) {
    for(int j = 0; j < a.getHeight(); j++) {
      if (g->get(p.x+i,p.y+j) == FULL) {
	if ((p.y+j % 2) == 0) {
	  output << "\t\t<block position=\"" << i << "," << j << "\"/>\n";
	} else {
	  output << "\t\t<block position=\"" << i+1 << "," << j << "\"/>\n";
	}
      }
    }
  }
}

static void write_initial(ofstream &output, Grid *g) {
  output << "\t<blockList color=\"100,100,100\" blocksize=\"1,5,1\">\n";
  //output << "\t\t<block position=\"3,0\"/>\n";
  write_configuration(output,g,g->getInitialArea());
  output << "\t</blockList>\n";
}

static void write_goal(ofstream &output, Grid *g) {
  output << "\t<targetGrid>\n";
  //output << "\t\t<block position=\"4,0\"/>\n";
  write_configuration(output,g,g->getGoalArea());		
  output << "\t</targetGrid>\n";
}

static void help() {
  cerr << program << " options:" << endl;
  cerr << "\t -o <name>\toutput xml file" << endl;
  cerr << "\t -n <# modules>\t # modules to generate" << endl;
  cerr << "\t -r <ratio>\tratio = width/height" << endl;
  cerr << "\t -h \t\thelp" << endl;
  exit(EXIT_SUCCESS);
}

static Grid* initGrid(int n, double r) {
  // ignore ratio for the moment
  
  // square grid for now
  int width = n;
  int height = n;

  // twice the grid (a side for the initial config, another for the goal)
  return new Grid(2*width,height);
}

static double euclideanDistance(Coordinate p1, Coordinate p2) {
  return sqrt(pow(p2.x-p1.x,2)+pow(p2.y-p1.y,2));
}

static bool areNeighbors(Coordinate p1, Coordinate p2) {
  return (euclideanDistance(p1,p2) <= sqrt(2));
}

static void generate(Grid *g,configuration_t c,int n) {
  int i = 0;
  int j = 0;
  int lastInserted = 0;
  srand (time(NULL));

  int illegalX = 0;

  Rectangle bound;

  if(c == INITIAL) {
    bound = g->getInitialArea();
  } else if (c == GOAL) {
    bound = g->getGoalArea();
  } else {
    cerr << "internal error" << endl;
  }
  
  while(n > 0) {
    int toInsert = INT_MAX;
    int inserted = 0;

    if (j == 0) {
      
      if(c == INITIAL) {
	Coordinate p = bound.getPBottomRight();
	i = p.x;
	illegalX = i;
      } else if (c == GOAL) {
        Coordinate p = bound.getPBottomLeft();
	i = p.x;
	illegalX = i;
      } else {
	cerr << "internal error" << endl;
      }
    
    } else {
    
      Coordinate p = bound.getPBottomLeft();
      int k = 0;
      do {
	k = rand() % bound.getWidth();
      } while (g->get(p.x+k,j-1) != FULL);
      i = p.x + k;
    }

    g->set(i,j,FULL);
    inserted++;

    cout << "toInsert... " << endl;
    cout << "bound: " << bound.getWidth() << endl;
    if (j == 0) {
     toInsert = n/2;
    } else {
      do {
	toInsert = rand() % bound.getWidth();
      }  while ((toInsert < 1) || (toInsert > lastInserted));
      
      if (toInsert > n) {
	toInsert = n;
      }
    }
    cout << "toInsert: " << toInsert << endl;
    //getchar();
    /*while(inserted < toInsert) {
      bool done = false;
      while (!done) {
	Coordinate p = bound.getPBottomLeft();
	int k = 0;
	do {
	  k = rand() % bound.getWidth();
	} while (g->get(p.x+k,j) != FULL);
	i = p.x + k;
	Coordinate left = Coordinate(i-1,j);
	Coordinate right = Coordinate(i+1,j);
	if (bound.contains(left) && (g->get(left.x,left.y)==EMPTY) && (left.x != illegalX) && ((left.y == 0) || (g->get(left.x,left.y-1)==FULL))) {
	  g->set(i,j,FULL);
	  done = true;
	} else if (bound.contains(right) && (g->get(right.x,right.y)==EMPTY) && (right.x != illegalX) && ((right.y == 0) || (g->get(right.x,right.y-1)==FULL))) { 
	  g->set(i,j,FULL);
	  done = true;
	}
      }
      inserted++;
      }*/
    n = n - inserted;
    lastInserted = inserted;
    j++;
    }
}
