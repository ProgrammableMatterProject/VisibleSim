/*
 * smartblock01.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "catom2D1BlockCode.h"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {

  createSimulator(argc, argv, Catoms2D1BlockCode::buildNewBlockCode);
  Scheduler *scheduler = getScheduler();
  getSimulator()->printInfo();
  scheduler->printInfo();
  BaseSimulator::getWorld()->printInfo();
  deleteSimulator();
  
  return(0);
}
