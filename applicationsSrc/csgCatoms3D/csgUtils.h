#ifndef CSGUTILS_H_
#define CSGUTILS_H_

#include "csg.h"
#include "color.h"

enum class CSG_T : unsigned char
{
    Difference = 0, Union, Translate, Scale, Rotate, Color, Cube, Cylinder, Sphere, END
};

class PositionInfo
{
public:
    bool inside;
    Color color;
    PositionInfo() : inside(false) {};
    PositionInfo(bool i) : inside(i) {};
    PositionInfo(bool i, Color c) : inside(i), color(c) {};
    bool isInside() { return inside; };
    Color getColor() { return color; };
};

class CsgUtils
{
    int csgBufferPos; // CSG binary position
    char *csgBuffer;
    int csgBufferSize;
    CsgNode csgTree;

public:
    void createCSG(int MAX_SIZE);
    void readFile(string path_to_file);
    void readCSGBuffer(char *csgBuffer, int csgBufferSize);
    CsgNode getCSGTree() { return csgTree; }
    char * getCSGBuffer() { return csgBuffer; }
    int getCSGBufferSize() { return csgBufferSize; }
    PositionInfo isInside(Vector3D catomPosition);

private:
    CsgNode readCSGNode();
    PositionInfo isInside(CsgNode &node, Vector3D basePosition, Color color, Vector3D catomPosition);
};

#endif /* CSGUTILS_H_ */

