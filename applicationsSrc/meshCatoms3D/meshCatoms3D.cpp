/*
 * meshCatoms3D.cpp
 *
 *  Created on: 26/06/2018
 *      Author: pthalamy
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "meshCatoms3DBlockCode.hpp"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

    try
    {
        createSimulator(argc, argv, MeshCatoms3DBlockCode::buildNewBlockCode);
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
