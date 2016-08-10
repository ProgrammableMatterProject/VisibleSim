#ifndef STOYUTILS_H_
#define STOYUTILS_H_
#include <vector>
#include "cell3DPosition.h"

class Brick
{
public:
    Cell3DPosition p1;
    Cell3DPosition p2;
};

class StoyUtils
{
    vector<Brick> bricks;

public:
    void readFile(string path_to_file);
    bool isInside(Cell3DPosition catomPosition);

    void setBricks(vector<Brick> b) { bricks = b; };
    vector<Brick> getBricks() { return bricks; };
};

#endif /* STOYUTILS_H_ */

