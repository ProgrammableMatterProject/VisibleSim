/*
 * greedySRCatoms3D.cpp
 *
 *  Created on: 06/11/17
 *      Author: pthalamy
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "meltSortGrowBlockCode.hpp"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

    try
    {
        createSimulator(argc, argv, MeltSortGrowBlockCode::buildNewBlockCode);
        getSimulator()->printInfo();
        BaseSimulator::getWorld()->printInfo();
        deleteSimulator();
    }
    catch(std::logic_error const& err)
    {
        cerr << err.what();
    }
    catch (char const* msg)
    {
        cerr << msg << endl;
    }
    catch (std::exception e) {
        cerr << "exeption" << endl;
    }

    cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return 0;
}
