#include <iostream>
#include <fstream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

int main(int argc,char **argv) {
    int initSquareLx=31,initSquareLy=30,
        finalSquareLx=30,finalSquareLy=30;
    int maxTimeSimulation=3000;

    if (argc>1) {
        initSquareLx = atoi(argv[1]);
        cout << "initSquareLx=" << initSquareLx << endl;
    }
    if (argc>2) {
        initSquareLy = atoi(argv[2]);
        cout << "initSquareLy=" << initSquareLy << endl;
    }
    if (argc>3) {
        finalSquareLx = atoi(argv[3]);
        cout << "finalSquareLx=" << finalSquareLx << endl;
    }
    if (argc>4) {
        finalSquareLy = atoi(argv[4]);
        cout << "finalSquareLy=" << finalSquareLy << endl;
    }
    if (argc>5) {
        maxTimeSimulation = atoi(argv[5]);
        cout << "maxTimeSimulation=" << maxTimeSimulation << endl;
    }


    int areaLx = 8+initSquareLx+finalSquareLx,
        areaLy = 8+initSquareLy+finalSquareLy;
    int cameraDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(50.0*M_PI/180.0);
    int srcDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(45.0*M_PI/180.0);


    char titre[1024];
    sprintf(titre,"configMakeSquare%dx%d_%dx%d.xml",initSquareLx,initSquareLy,finalSquareLx,finalSquareLy);

    ofstream fout;
    fout.open(titre);

    fout << "<?xml version=\"1.0\" standalone=\"no\" ?>"<< endl;
    fout << "<world gridSize=\""<< areaLx << "," << areaLy << "\" windowSize=\"1800,900\" maxSimulationTime=\"" << maxTimeSimulation << "mn\">" << endl;
	fout << "<camera target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"0,38," << cameraDist << "\" angle=\"50\"/>" << endl;
	fout << "<spotlight target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"-30,50," << srcDist << "\" angle=\"45\"/>" << endl;
	fout << "<blockList color=\"0,255,0\" blocksize=\"25.0,25.0,11.0\">" << endl;

    int ix,iy;
    for (iy=0; iy<initSquareLy; iy++) {
        fout << "<blocksLine line=\"" << initSquareLy+3-iy << "\" values=\"0000";
        for (ix=0; ix<initSquareLx; ix++) {
            fout << "1";
        }
        for (;ix<areaLx-4; ix++) {
            fout << "0";
        }
        fout << "\"/>" << endl;

    }
    fout << "</blockList>\n<targetGrid>" << endl;

    int ix0=4+initSquareLx-1,
        iy0=4+initSquareLy-1;
    for (iy=0; iy<finalSquareLy; iy++) {
        fout << "<targetLine line=\"" << finalSquareLy-1+iy0-iy << "\" values=\"";
        for (ix=0; ix<ix0; ix++) {
            fout << "0";
        }
        for (ix=0; ix<finalSquareLx; ix++) {
            fout << "1";
        }
        fout << "0000\"/>" << endl;
    }
    fout << "</targetGrid>" << endl;

// lecture du fichier capabilities.xml
    ifstream fin("capabilities.xml");
    char line[1024];
    while (!fin.eof()) {
        fin.getline(line,1024);
        fout << line << endl;
    }
    fout << "</world>\n";

    fout.close();
    return 0;
}
