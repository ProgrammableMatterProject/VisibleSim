#include <iostream>
#include "slidingCubesSimulator.h"
#include "slidingCubesBlockCode.h"
#include "distanceSCCode.h"
using namespace std;
using namespace SlidingCubes;

int main(int argc, char **argv) {
    createSimulator(argc, argv, DistanceSCCode::buildNewBlockCode);

    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
    BaseSimulator::deleteSimulator();
    return(0);
}
