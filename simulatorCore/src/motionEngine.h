#ifndef MOTION_ENGINE_H_
#define MOTION_ENGINE_H_

#include "tDefs.h"
#include "rate.h"

#define DEFAULT_MOTION_SPEED 0.1
#define CATOMS2D_MOTION_SPEED_MEAN 1.881119320574239116075
#define CATOMS2D_MOTION_SPEED_SD 0.01

namespace BaseSimulator {

class MotionEngine {
 protected:
  Rate *speed; //<! mm/s (beaucse mm is the distance unit in the simulator)
 public:
  MotionEngine();
  MotionEngine(Rate *s);
  ~MotionEngine();

  void setSpeed(Rate *s);
  double getSpeed();
  // ms
  Time getDuration(Distance distance);
};

}

#endif
