#include <iostream>
#include "robots/okteen/okteenSimulator.h"
#include "robots/okteen/okteenBlockCode.h"
#include "simpleColorCodeOkteen.h"

using namespace std;
using namespace Okteen;

int main(int argc, char **argv) {
    createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

    getSimulator()->printInfo();
    BaseSimulator::getWorld()->printInfo();
    deleteSimulator();
    return(0);
}
