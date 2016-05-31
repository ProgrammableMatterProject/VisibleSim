#include <iostream>
#include <fstream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "skeleton.h"

using namespace std;

int main(int argc,char **argv) {
    int initSquareLx=41,initSquareLy=41,
        conveyorBlocks=500,armLength=15;
    int maxTimeSimulation=3000,iAngle=45;
    double angle;

    if (argc>1) {
        initSquareLx = atoi(argv[1]);
        cout << "initSquareLx=" << initSquareLx << endl;
    }
    if (argc>2) {
        initSquareLy = atoi(argv[2]);
        cout << "initSquareLy=" << initSquareLy << endl;
    }
    if (argc>3) {
        conveyorBlocks = atoi(argv[3]);
        cout << "conveyorBlocks=" << conveyorBlocks << endl;
    }
    if (argc>4) {
        armLength = atoi(argv[4]);
        cout << "armLength=" << armLength << endl;
    }
    if (argc>5) {
        iAngle = atoi(argv[5]);
        cout << "iangle=" << iAngle << endl;
    }
    angle = iAngle*M_PI/180.0;
    if (argc>6) {
        maxTimeSimulation = atoi(argv[6]);
        cout << "maxTimeSimulation=" << maxTimeSimulation << endl;
    }



    int areaLx = 4+(max(initSquareLx,4*armLength)),
        areaLy = 15+initSquareLy+2*armLength;
    int cameraDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(50.0*M_PI/180.0);
    int srcDist = 25.0*sqrt(areaLx*areaLx+areaLy*areaLy)/tan(45.0*M_PI/180.0);

    Vecteur Origine(areaLx/2.0,areaLy-initSquareLy-3,0);
    Vecteur P(Origine[0],Origine[1]-armLength/2,0);
    Vecteur Q(P[0]+armLength*1.5*cos(angle),P[1]-armLength*1.5*sin(angle),0);
    Vecteur R(P[0]-armLength*1.5*cos(angle),P[1]-armLength*1.5*sin(angle),0);
    cout << "O=" << Origine << endl;
    cout << "P=" << P << endl;
    cout << "Q=" << Q << endl;
    /*Skeleton *S = new SkelPoint(Origine,6,-0.1);
    Skeleton *S0 = new SkelLine(Origine,P,3,-0.2);
    S->add(S0);
    Skeleton *S1 = new SkelPoint(P,3,-0.1);
    S0->add(S1);
    Skeleton *S2 = new SkelLine(P,Q,3,-0.2);
    S1->add(S2);
    Skeleton *S3 = new SkelPoint(Q,6,-0.1);
    S2->add(S3);*/
    Skeleton *S = new SkelLine(Origine,P,3,-0.2);
    Skeleton *S2 = new SkelLine(P,Q,3,-0.2);
    S->add(S2);
    Skeleton *S3 = new SkelLine(P,R,3,-0.2);
    S->add(S3);

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
    for (i=0; i<conveyorBlocks; i++) {
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
    sprintf(titre,"configMakeY%dx%d_%d_%d.xml",initSquareLx,initSquareLy,conveyorBlocks,armLength);

    ofstream fout;
    fout.open(titre);

    fout << "<?xml version=\"1.0\" standalone=\"no\" ?>"<< endl;
    fout << "<world gridSize=\""<< areaLx << "," << areaLy << "\" windowSize=\"1800,900\" maxSimulationTime=\"" << maxTimeSimulation << "mn\">" << endl;
	fout << "<camera target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"0,38," << cameraDist << "\" angle=\"50\"/>" << endl;
	fout << "<spotlight target=\""<< areaLx*25.0/2.0 << "," << areaLy*25.0/2.0 << ",0\" directionSpherical=\"-30,50," << srcDist << "\" angle=\"45\"/>" << endl;
	fout << "<blockList color=\"0,255,0\" blocksize=\"25.0,25.0,11.0\">" << endl;

    for (iy=0; iy<initSquareLy; iy++) {
        fout << "<blocksLine line=\"" << initSquareLy+1-iy << "\" values=\"00";
        for (ix=0; ix<areaLx/2-initSquareLx; ix++) {
            fout << "0";
        }
        for (ix=0; ix<initSquareLx; ix++) {
            fout << "1";
        }
        for (ix=areaLx/2; ix<areaLx; ix++) {
            fout << "0";
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
