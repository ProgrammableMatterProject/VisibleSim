#ifndef CATOMS2D_MOTION_ENGINE_H_
#define CATOMS2D_MOTION_ENGINE_H_

#include "utils/tDefs.h"
#include "comm/rate.h"

#define DEFAULT_MOTION_SPEED 0.1
#define CATOMS2D_MOTION_SPEED_MEAN 1.881119320574239116075
#define CATOMS2D_MOTION_SPEED_SD 0.01

namespace BaseSimulator {

class Catoms2DMotionEngine {
 protected:
  Rate *speed; //<! m/s
 public:
  Catoms2DMotionEngine();
  Catoms2DMotionEngine(Rate *s);
  ~Catoms2DMotionEngine();

  void setSpeed(Rate *s);
  double getSpeed();
  // ms
  Time getDuration(Distance distance);
};

}

#endif
