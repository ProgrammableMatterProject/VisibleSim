#ifndef MESHUTILS_H_
#define MESHUTILS_H_
#include "vecteur.h"
#include "mesh/world.h"

class MeshUtils
{

public:
    MeshWorld w;

    void readFile(string path_to_file);
    bool isInside(Vecteur catomPosition);

private:
};

#endif /* MESHUTILS_H_ */

