#include <iostream>
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsRotateCode.h"

using namespace std;
using namespace Datoms;

int main(int argc, char **argv) {
    try
    {
        createSimulator(argc, argv, DatomsRotateCode::buildNewBlockCode);
        getSimulator()->printInfo();
        BaseSimulator::getWorld()->printInfo();
        deleteSimulator();
    }
    catch(std::exception const& e)
    {
        cerr << "Uncaught exception: " << e.what();
    }

    return(0);
}
