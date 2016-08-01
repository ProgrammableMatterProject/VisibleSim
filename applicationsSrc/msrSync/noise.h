#ifndef NOISE_H
#define NOISE_H

//#include <pair>
#include <stdint.h>
#include <vector>
#include <string>

typedef double noise_t;
typedef std::pair<uint64_t, noise_t> referencePt_t;
typedef std::vector<referencePt_t> noiseSignal_t;

class Noise {
 private:
  std::vector<noiseSignal_t> noiseSignals;
  //static std::string directory;
  noise_t getNoise(uint64_t time, referencePt_t p1, referencePt_t p2);
  void print();
  void test();
 
 public:
  Noise();
  ~Noise();
  noise_t getNoise(unsigned int id, uint64_t time);
  unsigned int getNoiseId(unsigned int r);
};

#endif
