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
    static int getSimulationSeed();

    static intRNG getUniformIntRNG(ruint seed, rint min, rint max);
    static intRNG getUniformIntRNG(rint min, rint max) {
        return getUniformIntRNG(getSimulationSeed(), min, max);
    }

    static doubleRNG getUniformDoubleRNG(ruint seed, rdouble min, rdouble max);
    static doubleRNG getUniformDoubleRNG(rdouble min, rdouble max) {
        return getUniformDoubleRNG(getSimulationSeed(), min, max);
    }

    static doubleRNG getNormalDoubleRNG(ruint seed, rdouble mean, rdouble sd);
    static doubleRNG getNormalDoubleRNG(rdouble mean, rdouble sd) {
        return getNormalDoubleRNG(getSimulationSeed(), mean, sd);
    }
};

}
}

#endif
