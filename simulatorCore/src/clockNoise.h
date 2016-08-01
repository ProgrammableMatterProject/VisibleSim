
#ifndef CLOCK_NOISE_H_
#define CLOCK_NOISE_H

#include <cstdint>
#include <vector>
#include <string>

using namespace std;

namespace BaseSimulator {

typedef double clockNoise_t;
  
class ClockNoise {
  
private:
  void print();
  void test();
  
public:
  ClockNoise();
  virtual ~ClockNoise();
  
  virtual clockNoise_t getNoise(uint64_t simTime) = 0;
};

class GClockNoise : public ClockNoise {
 private:
  unsigned int seed;
  double mean;
  double sd;

 public:
  GClockNoise(unsigned int _seed, double _mean, double _sd);
  ~GClockNoise();

  clockNoise_t getNoise(uint64_t simTime);
};

class DClockNoise : public ClockNoise {
  typedef pair<uint64_t, clockNoise_t> referencePt_t;
  typedef vector<referencePt_t> noiseSignal_t;
  static vector<noiseSignal_t> noiseSignals;
 protected:
  unsigned int id;
 private:
  clockNoise_t getNoise(uint64_t time, referencePt_t p1, referencePt_t p2);
  
 public:
  DClockNoise(unsigned int seed);
  ~DClockNoise();
  clockNoise_t getNoise(uint64_t simTime);
  void print();
    
  static void loadData(vector<string> &files);
};

}

#endif
