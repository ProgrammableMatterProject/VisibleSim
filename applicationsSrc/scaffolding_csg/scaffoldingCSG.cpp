/**
 * @file   scaffoldingCSG.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jun 11 15:27:36 2019
 *
 * @brief
 *
 *
 */


#include <iostream>

#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "scaffoldingBlockCode.hpp"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
    cerr << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

    try
    {
        createSimulator(argc, argv, ScaffoldingBlockCode::buildNewBlockCode);
        getSimulator()->printInfo();
        BaseSimulator::getWorld()->printInfo();
        deleteSimulator();
    }
    catch(BaseSimulator::VisibleSimException const& e1)
    {
        cerr << "error: " << e1.what();
    }
    catch(std::exception const& e)
    {
        cerr << e.what();
    }
    catch (char const* msg)
    {
        cerr << msg;
    }

    cerr << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

    return 0;
}
