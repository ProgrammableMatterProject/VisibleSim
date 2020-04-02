#include <iostream>
#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"
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
