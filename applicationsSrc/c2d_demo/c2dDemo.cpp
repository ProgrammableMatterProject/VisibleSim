/**
 * @file   c2dDemo.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 09:49:48 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include "robots/catoms2D/catoms2DSimulator.h"
#include "robots/catoms2D/catoms2DBlockCode.h"
#include "c2dDemoBlockCode.hpp"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {
    try {
        createSimulator(argc, argv, C2dDemoBlockCode::buildNewBlockCode);
        getSimulator()->printInfo();
        BaseSimulator::getWorld()->printInfo();
        deleteSimulator();
    }
    catch(std::logic_error const& err) {
        cerr << err.what();
    }
    catch (char const* msg) {
        cerr << msg << endl;
    }

    return 0;
}
