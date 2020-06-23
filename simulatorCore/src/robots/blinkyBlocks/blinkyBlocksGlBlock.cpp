#include "blinkyBlocksGlBlock.h"

namespace BlinkyBlocks {

void BlinkyBlocksGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
    glPushMatrix();

    glTranslatef(position[0],position[1],position[2]);
    if (isHighlighted) {
        GLfloat n = 0.5+1.5*(1.0-(glutGet(GLUT_ELAPSED_TIME)%1000)/1000.0);
        GLfloat c[4];
        c[0]=GLfloat(color[0])*n/255.0;
        c[1]=GLfloat(color[1])*n/255.0;
        c[2]=GLfloat(color[2])*n/255.0;
        c[3]=1.0;
        ptrObj->setLightedColor(c);
    } else {
        ptrObj->setLightedColor(color);
    }
    ptrObj->glDraw();
    glPopMatrix();
}

}
