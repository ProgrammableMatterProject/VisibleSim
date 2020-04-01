/**
 * @file   nodes2DDemo.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:17:18 2019
 *
 * @brief
 *
 *
 */

#include <iostream>

#include "robots/nodes2D/nodes2DSimulator.h"
#include "robots/nodes2D/nodes2DBlockCode.h"
#include "nodes2DDemoBlockCode.hpp"

using namespace std;
using namespace Nodes2D;

int main(int argc, char **argv) {
    try {
        createSimulator(argc, argv, Nodes2DDemoBlockCode::buildNewBlockCode);
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
