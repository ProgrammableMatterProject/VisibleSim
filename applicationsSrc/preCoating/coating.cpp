/**
 * @file   coating.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct  9 16:56:16 2019
 *
 * @brief
 *
 *
 */

#include <iostream>

#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "coatingBlockCode.hpp"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
    try
    {
        createSimulator(argc, argv, CoatingBlockCode::buildNewBlockCode);
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

    return 0;
}
