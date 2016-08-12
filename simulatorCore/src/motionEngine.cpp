#include <cmath>
#include <iostream>
#include "motionEngine.h"

using namespace std;

namespace BaseSimulator {
  
MotionEngine::MotionEngine() {
  speed = new StaticRate(DEFAULT_MOTION_SPEED);
}

MotionEngine::MotionEngine(Rate *s) {
  speed = s;
}

MotionEngine::~MotionEngine() {
  delete speed;
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
