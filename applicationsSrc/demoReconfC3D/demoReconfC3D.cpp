#include <iostream>
#include "demoReconfC3DCode.h"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, DemoReconfCode::buildNewBlockCode);
	deleteSimulator();

	return(0);
}
