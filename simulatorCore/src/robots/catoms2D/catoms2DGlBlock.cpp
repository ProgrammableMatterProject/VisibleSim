#include "catoms2DGlBlock.h"

namespace Catoms2D {

void Catoms2DGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
    glPushMatrix();
    glTranslatef(position[0],position[1],position[2]);
    glRotatef(-angle,0,1,0);
    if (highlighted()) {
        GLfloat n = 0.5+1.5*(1.0-(glutGet(GLUT_ELAPSED_TIME)%1000)/1000.0);
        GLfloat c[4];
        c[0]=color[0]*n;
        c[1]=color[1]*n;
        c[2]=color[2]*n;
        c[3]=1.0;
        ptrObj->setLightedColor(c);
    } else {
        ptrObj->setLightedColor(color);
    }
    ptrObj->glDraw();
    glPopMatrix();
}

}
