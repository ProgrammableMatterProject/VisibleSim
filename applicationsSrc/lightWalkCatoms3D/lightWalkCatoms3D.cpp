/**
 * @file   lightWalkCatoms3D.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Mon Dec 10 15:27:06 2018
 *
 * @brief
 *
 *
 */

#include <iostream>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "lightWalkCatoms3DBlockCode.hpp"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
    cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

    try
    {
        createSimulator(argc, argv, LightWalkCatoms3DBlockCode::buildNewBlockCode,
                        // useSkewedFCCLattice
                        true);
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

    cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

    return 0;
}
