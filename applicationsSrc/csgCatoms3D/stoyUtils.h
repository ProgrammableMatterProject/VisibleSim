#ifndef STOYUTILS_H_
#define STOYUTILS_H_
#include <vector>
#include "math/vector3D.h"

class Brick
{
public:
    Vector3D p1;
    Vector3D p2;
};

class StoyUtils
{
    static vector<Brick> bricks;

public:
    void readFile(string path_to_file);
    bool isInside(Vector3D catomPosition);

    void setBricks(vector<Brick> b) { bricks = b; };
    vector<Brick> getBricks() { return bricks; };
};

#endif /* STOYUTILS_H_ */
