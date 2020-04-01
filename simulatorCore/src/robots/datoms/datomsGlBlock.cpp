/*!
 * \file datomsGlBlock.cpp
 * \brief deformable atoms gl
 * \date 28/01/2018
 * \author Benoît Piranda
 */
#include "robots/datoms/datomsGlBlock.h"

namespace Datoms {

void DatomsGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
    glPushMatrix();
    mat.glMultMatrix();
    if (isHighlighted) {
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
    glDisable(GL_CULL_FACE);
    if (color[3] > 0) ptrObj->glDraw(currentModel);
    glEnable(GL_CULL_FACE);
    glPopMatrix();
}

}
