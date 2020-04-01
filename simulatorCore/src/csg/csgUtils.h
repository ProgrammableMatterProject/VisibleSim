#ifndef CSGUTILS_H_
#define CSGUTILS_H_

#include "csg.h"
#include "utils/color.h"
#include "csgParser.h"

class CsgUtils
{
    int csgBufferPos; // CSG binary position
    char *csgBuffer;

public:
    ~CsgUtils();
    CSGNode* readFile(string path_to_file);
    CSGNode* readCSGBuffer(char *csgBuffer);
    char * getCSGBuffer() { return csgBuffer; }
private:
    CSGNode* readCSGNode();
};

#endif /* CSGUTILS_H_ */
