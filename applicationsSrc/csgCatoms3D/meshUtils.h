#ifndef MESHUTILS_H_
#define MESHUTILS_H_
#include "math/vector3D.h"
#include "mesh/world.h"

class MeshUtils
{

public:
    MeshWorld w;

    void readFile(string path_to_file);
    bool isInside(Vector3D catomPosition);

private:
};

#endif /* MESHUTILS_H_ */
