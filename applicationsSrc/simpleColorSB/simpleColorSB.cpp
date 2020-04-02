#include <iostream>
#include "robots/smartBlocks/smartBlocksSimulator.h"
#include "robots/smartBlocks/smartBlocksBlockCode.h"
#include "simpleColorCodeSB.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
    createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
    BaseSimulator::deleteSimulator();
    return(0);
}
