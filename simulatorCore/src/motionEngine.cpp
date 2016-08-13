#include <cmath>
#include <iostream>
#include <cassert>

#include "motionEngine.h"

using namespace std;

namespace BaseSimulator {
  
MotionEngine::MotionEngine() {
  speed = new StaticRate(DEFAULT_MOTION_SPEED);
}

MotionEngine::MotionEngine(Rate *s) {
  assert(s != NULL);
  speed = s;
}

MotionEngine::~MotionEngine() {
  delete speed;
}

void MotionEngine::setSpeed(Rate *s) {
  assert(s != NULL);
  delete speed;
  speed = s;
}
  
double MotionEngine::getSpeed() {
  return speed->get();
}

Time MotionEngine::getDuration(Distance distance) {
  double speed = getSpeed();
  double time = distance/speed;
  // cout << "speed (m/s) = " << speed << ", distance (m) = " << distance
  //     << ", time (s) = " << time << endl;
  Time t = time*pow(10,6);
  
  return t;
}

}
