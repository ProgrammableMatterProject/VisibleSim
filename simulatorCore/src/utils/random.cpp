#include <iostream>
#include "random.h"
#include "../base/simulator.h"

using namespace std;

namespace BaseSimulator {
namespace utils {

int Random::getSimulationSeed() {
    return Simulator::getSimulator()->getSimulationSeed();
}

doubleRNG Random::getUniformDoubleRNG(ruint seed, rdouble min, rdouble max) {
    uintRNG gen(seed);
    uniform_real_distribution<rdouble> dis (min,max);
    doubleRNG generator = bind(dis,gen);
    return generator;
}

doubleRNG Random::getNormalDoubleRNG(ruint seed, rdouble mean, rdouble sd) {
    uintRNG gen(seed);
    normal_distribution<rdouble> dis (mean,sd);
    doubleRNG generator = bind(dis,gen);
    return generator;
}

intRNG Random::getUniformIntRNG(ruint seed, rint min, rint max) {
    uintRNG gen(seed);
    uniform_int_distribution<rint> dis (min,max);
    intRNG generator = bind(dis,gen);
    return generator;
}

}
}
