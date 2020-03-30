#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "locomotionCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
    cout << "\033[1;33m" << "Starting RobotBlocks simulation (main) ..."
         << "\033[0m" << endl;

    try
    {
        createSimulator(argc, argv, LocomotionCode::buildNewBlockCode);
        getSimulator()->printInfo();
        BaseSimulator::getWorld()->printInfo();
        deleteSimulator();
    }
    catch(BaseSimulator::VisibleSimException const& e1)
    {
        cerr << "internal error: " << e1.what();
    }
    catch(std::exception const& e)
    {
        cerr << e.what();
    }
    catch (char const* msg)
    {
        cerr << msg;
    }

    return(0);
}
