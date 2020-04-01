/**
 * @file   nodeDemo.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:17:18 2019
 *
 * @brief
 *
 *
 */

#include <iostream>

#include "robots/hexanodes/hexanodesSimulator.h"
#include "robots/hexanodes/hexanodesBlockCode.h"
#include "hexanodesDemoBlockCode.hpp"

using namespace std;
using namespace Hexanodes;

int main(int argc, char **argv) {
    try {
        createSimulator(argc, argv, HexanodesDemoBlockCode::buildNewBlockCode);
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
