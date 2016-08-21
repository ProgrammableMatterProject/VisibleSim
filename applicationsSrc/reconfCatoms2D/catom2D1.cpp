/*
 * catoms2D1.cpp
 *
 *  Created on: April 2015
 *      Author: Andre
 */

#include <iostream>
#include <string>
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "catom2D1BlockCode.h"

using namespace std;
using namespace Catoms2D;

void parseCmdLine(int argc, char **argv) {
  double comm = -1;
  double motion = -1;
  // intercept -C and -M command line argument
  // warning: must be at the end of the command line!
  for (int i = 0; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'C') {
	comm = stod(argv[i+1]);
 	argv[i][0] = 0;
	argv[i][1] = 0;
      }
      if (argv[i][1] == 'M') {
	motion = stod(argv[i+1]);
	argv[i][0] = 0;
	argv[i][1] = 0;
      }
    }
  }

  if (comm > 0) {
    cerr << "Communication rate: "
	 << " mean = " << comm << ","
	 << " sd = " << comm*DEFAULT_SD_FACTOR
	 << endl;
  }

  if (motion > 0) {
    cerr << "Motion speed: "
	 << " mean = " << motion << ","
	 << " sd = " << motion*DEFAULT_SD_FACTOR
	 << endl;
  }
  
  Catoms2D1BlockCode::simParams.commRateMean = comm;
  Catoms2D1BlockCode::simParams.motionSpeedMean = motion;
}

int main(int argc, char **argv) {

  parseCmdLine(argc,argv);
  
  createSimulator(argc, argv, Catoms2D1BlockCode::buildNewBlockCode);
  getSimulator()->printInfo();
  BaseSimulator::getWorld()->printInfo();
  deleteSimulator();
  
  return(0);
}
