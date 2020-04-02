#ifndef polymer_h
#define polymer_h

#include <vector>
#include "gui/shaders.h"

using namespace std;

#define MASSE	1100
#define GRAVITE	9.81
#define RAIDEUR1 1000000
#define AMORT	0.025


class Polymer {
        int _lx,_ly,_sub; // r�solution du mod�le physique
  // tableau contenant toutes les informations g�om�triques
    GLuint *_tabIndices; // tableau d'indices des faces graphique
    float *_tabZ,*_tabZ_1,*_tabVitesseZ; // tableaux de position et vitesse des masses de la grille de simulation
    float _dx,_dy;
    float _radius; // radius of obstacles

public :
    vector <Vector3D> tabPt;
    int _nx,_ny; // r�solution de la g�om�trie
    GLfloat *_tabGeom; // tableau de coordonn�es graphique

    Polymer(int,int,int,float,float,float,float);
    ~Polymer();
    void glDraw();
    float positionInstant(float dt);
    void calculerPolymer();
    bool collision(const Vector3D pos);
};

#endif
