#ifndef STOYUTILS_H_
#define STOYUTILS_H_
#include <vector>
#include "vecteur.h"

class Brick
{
public:
    Vecteur p1;
    Vecteur p2;
};

class StoyUtils
{
    vector<Brick> bricks;

public:
    void readStoyFile(string path_to_file);
    bool isInside(Vecteur catomPosition);

    void setBricks(vector<Brick> b) { bricks = b; };
    vector<Brick> getBricks() { return bricks; };
};

#endif /* STOYUTILS_H_ */

