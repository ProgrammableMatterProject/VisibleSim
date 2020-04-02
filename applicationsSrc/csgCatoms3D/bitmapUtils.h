#ifndef BITMAPUTILS_H_
#define BITMAPUTILS_H_
#include <vector>
#include "grid/cell3DPosition.h"

class BitmapUtils
{
    static string bitmap;

public:
    void readFile(string path_to_file);
    bool isInside(Cell3DPosition catomPosition, int side_size);

    void setBitmap(string bmp) { bitmap = bmp; };
    string getBitmap() { return bitmap; };
};

#endif /* BITMAPUTILS_H_ */
