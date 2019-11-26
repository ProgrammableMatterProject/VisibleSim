#include <iostream>
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "simpleReconfC2DCode.h"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {
    createSimulator(argc, argv, SimpleReconfC2DCode::buildNewBlockCode);

    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
    deleteSimulator();
    return(0);
}
