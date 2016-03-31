#ifndef CSGUTILS_H_
#define CSGUTILS_H_

#include "csg.h"

enum class Node_T : unsigned char
{
    Difference, Union, Translate, Rotate, Cube, Cylinder, Sphere, END
};

class CsgUtils
{
    int csgBufferPos; // CSG binary position
    char *csgBuffer;
    int csgBufferSize;
    CsgNode csgTree;

public:
    void createCSG(int MAX_SIZE);
    void readCSGFile(string path_to_file);
    void readCSGBuffer(char *csgBuffer, int csgBufferSize);
    CsgNode getCSGTree() { return csgTree; }
    char * getCSGBuffer() { return csgBuffer; }
    int getCSGBufferSize() { return csgBufferSize; }
    bool isInCSG(Vecteur catomPosition);

private:
    CsgNode readCSGNode();
    bool isInCSG(CsgNode &node, Vecteur basePosition, Vecteur catomPosition);
};

#endif /* CSGUTILS_H_ */

