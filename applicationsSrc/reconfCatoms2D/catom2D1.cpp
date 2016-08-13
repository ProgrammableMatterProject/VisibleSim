/*
 * smartblock01.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
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
  // Locate -k command line argument
  for (int i = 0; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'C') {
	comm = stod(argv[i+1]);
      }
      if (argv[i][1] == 'M') {
	motion = stod(argv[i+1]);
      }
    }
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
