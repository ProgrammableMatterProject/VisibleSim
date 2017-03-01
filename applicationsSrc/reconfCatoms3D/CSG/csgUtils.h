#ifndef CSGUTILS_H_
#define CSGUTILS_H_

#include "cell3DPosition.h"
#include "csg.h"

class CsgUtils
{

public:
    static CSGNode* csgRoot;
    static BoundingBox boundingBox;
    static Vector3D getWorldPosition(Cell3DPosition);
    static void fixBoundingBox();
    static void init(const string &file);
    static bool isInside(const Vector3D &);
    static bool isInside(const Cell3DPosition &);
};

#endif /* CSGUTILS_H_ */

