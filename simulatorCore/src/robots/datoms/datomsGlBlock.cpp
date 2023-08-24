/*!
 * \file datomsGlBlock.cpp
 * \brief deformable atoms gl
 * \date 28/01/2018
 * \author Benoît Piranda
 */
#include <iostream>
#include "datomsGlBlock.h"

namespace Datoms {

void DatomsGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
    glPushMatrix();
    mat.glMultMatrix();
    if (highlighted) {
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
    //if (color[3] > 0) ptrObj->glDraw(currentModel);
    glRotatef(45.0f,0.0f,0.0f,1.0f);
    glRotatef(45.0f,0.0f,1.0f,0.0f);
    ptrObj->glDraw(1);
    glEnable(GL_CULL_FACE);
    glPopMatrix();
}

string DatomsGlBlock::getPopupInfo() const {
    string out;
    Cell3DPosition res;
    res.pt[2] = round((2 * position[2]) / (M_SQRT2 * 10) - 0.5);
    res.pt[1] = round(position[1] / 10 - 0.5 - res.pt[2] / 2.0);
    res.pt[0] = round(position[0] / 10 - 0.5 - res.pt[2] / 2.0);
    out = to_string(blockId) + " - (" + to_string(res[0]) + "," + to_string(res[1]) + "," + to_string(res[2]) + ")";
    return out;
}


}
