#ifndef CSGUTILS_H_
#define CSGUTILS_H_

#include "csg.h"
#include "color.h"

enum class CSG_T : unsigned char
{
    Difference = 0, Union, Intersection, Translate, Scale, Rotate, Color, Cube, Cylinder, Sphere, END
};

class CsgUtils
{
    int csgBufferPos; // CSG binary position
    char *csgBuffer;
    int csgBufferSize;

public:
    //void createCSG(int MAX_SIZE);
    CSGNode* readFile(string path_to_file);
    CSGNode* readCSGBuffer(char *csgBuffer, int csgBufferSize);
    char * getCSGBuffer() { return csgBuffer; }
    int getCSGBufferSize() { return csgBufferSize; }
private:
    CSGNode* readCSGNode();
};

#endif /* CSGUTILS_H_ */

