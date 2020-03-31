/*
 * robot02.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "slidingCubesSimulator.h"
#include "slidingCubesBlockCode.h"
#include "robot02BlockCode.h"

using namespace std;
using namespace SlidingCubes;

int main(int argc, char **argv) {
    cout << "\033[1;33m" << "Starting SlidingCubes simulation (main) ..." << "\033[0m" << endl;

    createSimulator(argc, argv, Robot02BlockCode::buildNewBlockCode);
    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
/*
    scheduler->start(SCHEDULER_MODE_FASTEST);

    scheduler->waitForSchedulerEnd();
*/
    deleteSimulator();

    return(0);
}
