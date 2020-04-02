#ifndef RANDOM_H_
#define RANDOM_H_

#include <functional>
#include <random>

namespace BaseSimulator {
  
typedef std::mt19937 uintRNG;
typedef std::mt19937::result_type ruint;
 
typedef int rint;
typedef std::function<rint()> intRNG;
 
typedef double rdouble;
typedef std::function<rdouble()> doubleRNG;

namespace utils {
  
class Random {
 public:
  static intRNG getUniformIntRNG(ruint seed, rint min, rint max);
  static doubleRNG getUniformDoubleRNG(ruint seed, rdouble min, rdouble max);
  static doubleRNG getNormalDoubleRNG(ruint seed, rdouble mean, rdouble sd);
};

}
}
 
#endif
