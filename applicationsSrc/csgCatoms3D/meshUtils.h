#ifndef MESHUTILS_H_
#define MESHUTILS_H_
#include "cell3DPosition.h"
#include "mesh/world.h"

class MeshUtils
{

public:
    MeshWorld w;

    void readFile(string path_to_file);
    bool isInside(Cell3DPosition catomPosition);

private:
};

#endif /* MESHUTILS_H_ */

