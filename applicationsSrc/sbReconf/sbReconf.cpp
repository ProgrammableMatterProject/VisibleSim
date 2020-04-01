/*
 * smartblock01.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "robots/smartBlocks/smartBlocksSimulator.h"
#include "robots/smartBlocks/smartBlocksBlockCode.h"
#include "sbReconfBlockCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
    cout << "\033[1;33m" << "Starting Smart Blocks simulation (main) ..." << "\033[0m" << endl;

    createSimulator(argc, argv, SbReconfBlockCode::buildNewBlockCode);
    deleteSimulator();

    return(0);
}
