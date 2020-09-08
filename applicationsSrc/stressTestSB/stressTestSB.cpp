#include <iostream>

#include "robots/smartBlocks/smartBlocksSimulator.h"
#include "robots/smartBlocks/smartBlocksBlockCode.h"

#include "stressTestSBBlockCode.hpp"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
    try
    {
        createSimulator(argc, argv, StressTestSBBlockCode::buildNewBlockCode);
        getSimulator()->printInfo();
        BaseSimulator::getWorld()->printInfo();
        deleteSimulator();
    }
    catch(std::exception const& e)
    {
        cerr << "Uncaught exception: " << e.what();
    }

    return 0;
}
