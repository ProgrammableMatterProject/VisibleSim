#ifndef SIMULATION_PARAMETERS_H_
#define SIMULATION_PARAMETERS_H_

#define DEFAULT_SD_FACTOR 0.01 //1%
//#define DEFAULT_SD_FACTOR 0.05 //5%
//#define DEFAULT_SD_FACTOR 0.1 //10%

class SimulationParameters {
 public:
  double commRateMean;
  double motionSpeedMean;
};

#endif
