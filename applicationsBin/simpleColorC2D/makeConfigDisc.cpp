#include <iostream>
#include <fstream>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <list>

using namespace std;
const double M_SQRT3_2 = sqrt(3.0)/2.0;

class module {
public :
  int x,y;
  int indice;
  module(int cx,int cy):x(cx),y(cy),indice(0) {};
  bool operator < (const module& comp) {
    return indice < comp.indice;
  }
};

bool compare(const module* m1, const module* m2) {
  return m1->indice < m2->indice;
}


int main(int argc,char **argv) {
    int radius=10,radiusInterne=0;
    int maxTimeSimulation=3000;

    if (argc>1) {
        radius = atoi(argv[1]);
        cout << "radius=" << radius << endl;
    }
    if (argc>2) {
        radiusInterne = atoi(argv[2]);
        cout << "radiusInterne=" << radiusInterne << endl;
    }
    if (argc>3) {
        maxTimeSimulation = atoi(argv[3]);
        cout << "maxTimeSimulation=" << maxTimeSimulation << endl;
    }

    int areaLx = 2*radius,
        areaLy = 1.0,
        areaLz = ceil(2*radius/M_SQRT3_2);
    int cameraDist = 3.50*radius/tan(50.0*M_PI/180.0);
    int srcDist = 3.5*radius/tan(45.0*M_PI/180.0);

    char titre[1024];
    sprintf(titre,"configMakeDisc%dx%d.xml",radius,radiusInterne);

    ofstream fout;
    fout.open(titre);

    fout << "<?xml version=\"1.0\" standalone=\"no\" ?>"<< endl;
    fout << "<world gridSize=\""<< areaLx+1 << "," << areaLz+1 << "\" windowSize=\"1800,900\" maxSimulationTime=\"" << maxTimeSimulation << "mn\">" << endl;
  	fout << "<camera target=\""<< radius << "," << radius*M_SQRT3_2 <<"\" directionSpherical=\"0,38," << cameraDist << "\" angle=\"50\" far=" << cameraDist*2 << "/>" << endl;
  	fout << "<spotlight target=\""<< radius << "," << radius*M_SQRT3_2 << "\" directionSpherical=\"-30,50," << srcDist << "\" angle=\"45\"/>" << endl;
  	fout << "<blockList color=\"128,128,128\" blocksize=\"1,5,1\">" << endl;

    list<module*> modules;

    double x,z,dx,dz,d;
    int n=0;
    for (size_t iz = 0; iz < areaLz; iz++) {
      z = 0.5*M_SQRT3_2 + M_SQRT3_2*iz;
      for (size_t ix = 0; ix < areaLx; ix++) {
        x = (iz%2)?0.5+ix:ix;
        dx = x-radius;
        dz = z-radius;
        d = sqrt(dx*dx+dz*dz);
        //cout << "(" << ix << "," << iy << "," << iz << ") -> (" << x << "," << y << "," << z << ")" << endl;
        if (d<radius && d>radiusInterne) {
          modules.push_front(new module(ix,iz));
//          fout << "<block position=\"" << ix << ","<< iz << "\" orientation=\"0\" />" << endl;
          n++;
        }
      }
    }

    int *tabIndices = new int[n];
    for (size_t i=0; i<n; i++) {
      tabIndices[i]=i+1;
    }

    list<module*>::const_iterator ci = modules.begin();
    int ind;
    while (ci!=modules.end()) {
      ind = rand()%n;
      cout << ind << " -> ";
      while (tabIndices[ind]==0) {
        ind = (ind+1)%n;
      }
      (*ci)->indice = ind;
      tabIndices[ind]=0;
      cout << ind << endl;
      ci++;
    }

    modules.sort(compare);

    ci = modules.begin();
    while (ci!=modules.end()) {
      fout << "<block position=\"" << (*ci)->x << ","<< (*ci)->y << "\" orientation=\"0\" /> <!-- " << (*ci)->indice << " -->" << endl;
      delete (*ci);
      ci++;
    }

    cout << "nbre catoms:" << n << endl;

    fout << "</blockList>" << endl;
    fout << "</world>\n";
    fout.close();
    return 0;
}
