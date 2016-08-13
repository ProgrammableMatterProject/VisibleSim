#ifndef MOTION_ENGINE_H_
#define MOTION_ENGINE_H_

#include "tDefs.h"
#include "rate.h"

#define DEFAULT_MOTION_SPEED 0.1
#define CATOMS2D_MOTION_SPEED 1.67

namespace BaseSimulator {

class MotionEngine {
 protected:
  Rate *speed; //<! m/s
 public:
  MotionEngine();
  MotionEngine(Rate *s);
  ~MotionEngine();
  
  double getSpeed();
  // ms
  Time getDuration(Distance distance);
};

}

#endif
