#include <iostream>
#include <fstream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

using namespace std;

int main(int argc,char **argv) {
    char titrePPM[64],ligne[64];
    int maxTimeSimulation=300000,areaLx,areaLy;

    if (argc>1) {
        strncpy(titrePPM,argv[1],63);
        cout << "titre=" << titrePPM << endl;
    }

    ifstream fppm(titrePPM);
    fppm.getline(ligne,64);
    fppm.getline(ligne,64);
    fppm >> areaLx >> areaLy;
    char ch;
    fppm.read(&ch,1);
    do {
        fppm.read(&ch,1);
        cout << int(ch) << ":" << ch << ",";
    } while (ch!=10 && ch!=13);

    cout << areaLx << "," << areaLy << endl;

    uint8_t *ppm = new uint8_t[areaLx*areaLy*3];
    fppm.read((char*)ppm,areaLx*areaLy*3);

    int cameraDist = 2200;//25.0*areaLx/tan(50.0*M_PI/180.0);
    int srcDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(45.0*M_PI/180.0);

    int *grid = new int[areaLx * areaLy], *ptr=grid;
    int i=areaLx*areaLy;
    int v;
    while (i--) {

        v=(*ppm==0)?0:1;
        ppm++;
        v+=(*ppm==0)?0:2;
        ppm++;
        v+=(*ppm==0)?0:4;
        ppm++;
        *ptr++=v;
    }

    char titre[1024];
    sprintf(titre,"configMake%s.xml",titrePPM);

    ofstream fout;
    fout.open(titre);

    fout << "<?xml version=\"1.0\" standalone=\"no\" ?>"<< endl;
    fout << "<world gridSize=\""<< areaLx << "," << areaLy << "\" windowSize=\"1900,800\" maxSimulationTime=\"" << maxTimeSimulation << "mn\">" << endl;
	fout << "<camera target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"0,90," << cameraDist << "\" angle=\"45\"/>" << endl;
	fout << "<spotlight target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"-30,50," << srcDist << "\" angle=\"45\"/>" << endl;
	fout << "<blockList color=\"0,255,0\" blocksize=\"25.0,25.0,11.0\">" << endl;

int ix,iy;
    for (iy=0; iy<areaLy; iy++) {
        fout << "<blocksLine line=\"" << areaLy-1-iy << "\" values=\"";
        for (ix=0; ix<areaLx; ix++) {
            fout << ((grid[ix+iy*areaLx]&1)==1)?1:0;
        }
        fout << "\"/>" << endl;

    }
    fout << "</blockList>\n<targetGrid>" << endl;

/*    int ix0=2+initSquareLx-overlapLx,
        iy0=2+initSquareLy-overlapLy;
    for (iy=0; iy<finalSquareLy; iy++) {
        fout << "<targetLine line=\"" << finalSquareLy-1+iy0-iy << "\" values=\"";
        for (ix=0; ix<ix0; ix++) {
            fout << "0";
        }
        for (ix=0; ix<finalSquareLx; ix++) {
            fout << "1";
        }
        fout << "00\"/>" << endl;
    }*/

    for (iy=0; iy<areaLy; iy++) {
        fout << "<targetLine line=\"" << areaLy-iy-1 << "\" values=\"";
        for (ix=0; ix<areaLx; ix++) {
            fout << ((grid[ix+iy*areaLx]&2)==2)?1:0 ;
//fout << grid[ix+iy*areaLx];
        }
        fout << "\"/>" << endl;
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
