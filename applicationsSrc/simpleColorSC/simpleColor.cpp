#include <iostream>
#include "slidingCubesSimulator.h"
#include "slidingCubesBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace SlidingCubes;

int main(int argc, char **argv) {
    createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
    deleteSimulator();
    return(0);
}
