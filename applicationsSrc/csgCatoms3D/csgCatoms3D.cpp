/*
 * csgCatoms3D.cpp
 *
 *  Created on: 06 August 2015
 *  Author: Thadeu
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "csgCatoms3DBlockCode.h"
#include <ctime>

using namespace std;
using namespace Catoms3D;


void saveBitmap() {
    fstream bitmapFile("bitmap.out", fstream::out);
    unsigned char c;
    for (int i = 0; i < 27000; i+=8) {
        c = 0;
        for (int j = 0; j < 8; j++) {
            if (CsgCatoms3DBlockCode::bitmap[i+j]) {
                unsigned char d = 0;
                d = 1 << j;
                c = c | d;
            }
        }
        bitmapFile << c;
    }
    bitmapFile.close();
}

void printBenchmark(int numberOfBlocks) {
	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
    cout << "Difference stoy = " << CsgCatoms3DStats::difference_stoy << endl;
    cout << "Difference mesh = " << CsgCatoms3DStats::difference_mesh << endl;
    cout << "Difference bitmap = " << CsgCatoms3DStats::difference_bitmap << endl;
    cout << "Total catoms in csg = " << CsgCatoms3DStats::total_csg << endl;
    cout << "bitmap_time_elapsed = " << CsgCatoms3DStats::bitmap_time_elapsed << endl;
    cout << "csg_time_elapsed = " << CsgCatoms3DStats::csg_time_elapsed << endl;
    cout << "stoy_time_elapsed = " << CsgCatoms3DStats::stoy_time_elapsed << endl;
    cout << "mesh_time_elapsed = " << CsgCatoms3DStats::mesh_time_elapsed << endl;
    cout << "total number of modules = " << numberOfBlocks << endl;
}

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, CsgCatoms3DBlockCode::buildNewBlockCode);
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
/*
	scheduler->start(SCHEDULER_MODE_FASTEST);
	scheduler->waitForSchedulerEnd();
*/
    int numberOfBlocks = BaseSimulator::getWorld()->getSize();
	deleteSimulator();

    printBenchmark(numberOfBlocks);

	return(0);
}
