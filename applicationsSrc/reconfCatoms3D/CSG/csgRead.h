#ifndef CSGREAD_H_
#define CSGREAD_H_

#include "csg.h"
#include "color.h"
#include "cell3DPosition.h"

enum class CSG_T : unsigned char
{
    Difference = 0, Union, Intersection, Translate, Scale, Rotate, Color, Cube, Cylinder, Sphere, END
};

class CsgRead
{
private:
    int csgBufferPos; // CSG binary position
    char *csgBuffer;
    int csgBufferSize;
    CSGNode* readCSGNode();

public:
    CSGNode* readFile(string path_to_file);
    CSGNode* readCSGBuffer(char *csgBuffer, int csgBufferSize);
    char * getCSGBuffer() { return csgBuffer; }
    int getCSGBufferSize() { return csgBufferSize; }
};

#endif /* CSGREAD_H_ */


