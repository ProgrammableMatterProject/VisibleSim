#include <iostream>
#include "robots/datoms/datomsSimulator.h"
#include "robots/datoms/datomsBlockCode.h"
#include "datomsTestbedCode.h"

using namespace std;
using namespace Datoms;

int main(int argc, char **argv) {
    cout << "\033[1;33m" << "Starting Datom simulation (main) ..." << "\033[0m" << endl;

    createSimulator(argc, argv, DatomsTestbedCode::buildNewBlockCode);
    Scheduler *scheduler = getScheduler();

    getSimulator()->printInfo();
    scheduler->printInfo();
    BaseSimulator::getWorld()->printInfo();
    deleteSimulator();
    return(0);
}
