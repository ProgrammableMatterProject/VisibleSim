

#include <iostream>

#include "compressFullRangeBlockCode.hpp"
#include "robots/slidingCubes/slidingCubesBlockCode.h"
#include "robots/slidingCubes/slidingCubesSimulator.h"

using namespace std;
using namespace SlidingCubes;

int main(int argc, char** argv) {
	try {
		createSimulator(argc, argv,
		                CompressFullRangeBlockCode::buildNewBlockCode);
		getSimulator()->printInfo();
		BaseSimulator::getWorld()->printInfo();
		deleteSimulator();
	} catch (std::logic_error const& err) {
		cerr << err.what();
	} catch (char const* msg) {
		cerr << msg << endl;
	}

	return 0;
}
