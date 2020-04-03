#include <iostream>

#include "robots/<<moduleNameLc>>/<<moduleNameLc>>Simulator.h"
#include "robots/<<moduleNameLc>>/<<moduleNameLc>>BlockCode.h"

#include "<<appNameLc>>BlockCode.hpp"

using namespace std;
using namespace <<moduleName>>;

int main(int argc, char **argv) {
    try
    {
        createSimulator(argc, argv, <<appName>>BlockCode::buildNewBlockCode);
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
