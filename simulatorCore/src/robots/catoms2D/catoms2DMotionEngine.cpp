#include <cmath>
#include <iostream>
#include <cassert>

#include "robots/catoms2D/catoms2DMotionEngine.h"

//#define SPEED_DEBUG

using namespace std;

namespace BaseSimulator {

Catoms2DMotionEngine::Catoms2DMotionEngine() {
  speed = new StaticRate(DEFAULT_MOTION_SPEED);
}

Catoms2DMotionEngine::Catoms2DMotionEngine(Rate *s) {
  assert(s != NULL);
  speed = s;
}

Catoms2DMotionEngine::~Catoms2DMotionEngine() {
  delete speed;
}

void Catoms2DMotionEngine::setSpeed(Rate *s) {
  assert(s != NULL);
  delete speed;
  speed = s;
}

double Catoms2DMotionEngine::getSpeed() {
  return speed->get();
}

Time Catoms2DMotionEngine::getDuration(Distance distance) {
  double speed = getSpeed(); // mm/s
  double time = distance/speed; // s

#ifdef SPEED_DEBUG
  cerr << "speed (mm/s) = " << speed << ", distance (mm) = " << distance
       << ", time (s) = " << time << endl;
#endif

  Time t = time*pow(10,6); // us
  return t;
}

}
