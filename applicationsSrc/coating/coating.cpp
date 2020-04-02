#include <iostream>

#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"

#include "coatingBlockCode.hpp"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
    try
    {
        createSimulator(argc, argv, CoatingBlockCode::buildNewBlockCode,
                        // useSkewedFCCLattice
                        true);
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
