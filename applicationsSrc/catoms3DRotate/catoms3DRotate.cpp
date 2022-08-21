#include <iostream>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "catoms3DRotateCode.h"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
    cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

    createSimulator(argc, argv, Catoms3DRotateCode::buildNewBlockCode);
    deleteSimulator();

    return(0);
}
