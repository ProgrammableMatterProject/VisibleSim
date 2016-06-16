#include <iostream>
#include <fstream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "skeleton.h"

using namespace std;

int main(int argc,char **argv) {
    int line1Blocks=500,line2Blocks=450,armLength=15;
    int maxTimeSimulation=30000,iAngle=90;
    double angle;

    if (argc>1) {
        line1Blocks = atoi(argv[1]);
        cout << "line1Blocks=" << line1Blocks << endl;
    }
    if (argc>2) {
        line2Blocks = atoi(argv[2]);
        cout << "line2Blocks=" << line2Blocks << endl;
    }
    if (argc>3) {
        armLength = atoi(argv[3]);
        cout << "armLength=" << armLength << endl;
    }
    if (argc>4) {
        iAngle = atoi(argv[4]);
        cout << "iangle=" << iAngle << endl;
    }
    angle = iAngle*M_PI/180.0;
    if (argc>5) {
        maxTimeSimulation = atoi(argv[5]);
        cout << "maxTimeSimulation=" << maxTimeSimulation << endl;
    }


    int areaLx = 32+2*armLength,
        areaLy = areaLx;
    int cameraDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(50.0*M_PI/180.0);
    int srcDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(45.0*M_PI/180.0);

    Vecteur Origine(areaLx/2.0-armLength,areaLy/2.0+armLength,0);
    Vecteur P(Origine[0]+2*armLength,Origine[1],0);
    Vecteur Q(areaLx/2.0,P[1]-5,0);
    Vecteur R(Q[0]-(2*armLength-5)*cos(angle),Q[1]-(2*armLength-5)*sin(angle),0);
    cout << "O=" << Origine << endl;
    cout << "P=" << P << endl;
    cout << "Q=" << Q << endl;
    Skeleton *S = new SkelLine(Origine,P,3,-0.2);

    float *grid = new float[areaLx * areaLy], *ptr=grid;
    int i=areaLx*areaLy;
    while (i--) {
        *ptr++=0.0f;
    }

    int ix,iy;
    for (iy=0; iy<areaLy; iy++) {
        for (ix=0; ix<areaLx; ix++) {
            Origine.set(ix,iy,0);
            grid[iy*areaLx+ix] = S->potentiel(Origine);
        }
    }

    float maxi=0,s;
    int xmaxi,ymaxi;
    for (i=0; i<line1Blocks; i++) {
        maxi=0;
        for (iy=0; iy<areaLy; iy++) {
            for (ix=0; ix<areaLx; ix++) {
                s=grid[iy*areaLx+ix];
                if (s>=maxi) {
                    xmaxi = ix;
                    ymaxi = iy;
                    maxi = s;
                }
            }
        }
        grid[ymaxi*areaLx+xmaxi]=-1;
    }
    cout << maxi << endl;

    char titre[1024];
    sprintf(titre,"configMakeLine%d_%d_%d.xml",line1Blocks,line2Blocks,armLength);

    ofstream fout;
    fout.open(titre);

    fout << "<?xml version=\"1.0\" standalone=\"no\" ?>"<< endl;
    fout << "<world gridSize=\""<< areaLx << "," << areaLy << "\" windowSize=\"1800,900\" maxSimulationTime=\"" << maxTimeSimulation << "mn\">" << endl;
	fout << "<camera target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"0,38," << cameraDist << "\" angle=\"50\"/>" << endl;
	fout << "<spotlight target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"-30,50," << srcDist << "\" angle=\"45\"/>" << endl;
	fout << "<blockList color=\"0,255,0\" blocksize=\"25.0,25.0,11.0\">" << endl;

    for (iy=0; iy<areaLy; iy++) {
        fout << "<blocksLine line=\"" << areaLy+1-iy << "\" values=\"";
        for (ix=0; ix<areaLx; ix++) {
            fout << (grid[ix+iy*areaLx]==-1)?1:0 ;
//fout << grid[ix+iy*areaLx];
        }
        fout << "\"/>" << endl;
    }
    fout << "</blockList>\n<targetGrid>" << endl;

    delete S;
    S = new SkelLine(Q,R,3,-0.2);
    ptr=grid;
    i=areaLx*areaLy;
    while (i--) {
        *ptr++=0.0f;
    }

    for (iy=0; iy<areaLy; iy++) {
        for (ix=0; ix<areaLx; ix++) {
            Origine.set(ix,iy,0);
            grid[iy*areaLx+ix] = S->potentiel(Origine);
        }
    }

    maxi=0;
    for (i=0; i<line2Blocks; i++) {
        maxi=0;
        for (iy=0; iy<areaLy; iy++) {
            for (ix=0; ix<areaLx; ix++) {
                s=grid[iy*areaLx+ix];
                if (s>=maxi) {
                    xmaxi = ix;
                    ymaxi = iy;
                    maxi = s;
                }
            }
        }
        grid[ymaxi*areaLx+xmaxi]=-1;
    }
    cout << maxi << endl;


    for (iy=0; iy<areaLy; iy++) {
        fout << "<targetLine line=\"" << areaLy-iy-1 << "\" values=\"";
        for (ix=0; ix<areaLx; ix++) {
            fout << (grid[ix+iy*areaLx]==-1)?1:0 ;
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

    delete S;

    fout.close();
    return 0;
}
