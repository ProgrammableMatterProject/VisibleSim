/*
 * multiCoresBlock.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "multiCoresBlock.h"

using namespace std;

namespace MultiCores {

MultiCoresBlock::MultiCoresBlock(int bId, MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*)) : BuildingBlock(bId){
	cout << "MultiCoresBlock constructor" << endl;
	buildNewBlockCode = multiCoreBlockCodeBuildingFunction;
	blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
}



MultiCoresBlock::~MultiCoresBlock() {
	cout << "MultiCoresBlock destructor" << endl;
}

}
