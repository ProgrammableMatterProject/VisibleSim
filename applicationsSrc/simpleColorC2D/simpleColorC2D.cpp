#include <iostream>
#include "robots/catoms2D/catoms2DSimulator.h"
#include "robots/catoms2D/catoms2DBlockCode.h"
#include "simpleColorCodeC2D.h"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {
    createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
    deleteSimulator();
    return(0);
}
