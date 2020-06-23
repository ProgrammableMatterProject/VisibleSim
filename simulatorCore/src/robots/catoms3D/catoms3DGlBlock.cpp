#include "catoms3DGlBlock.h"
#include "catoms3DBlock.h"
#include "catoms3DWorld.h"

namespace Catoms3D {

void Catoms3DGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
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
    if (color[3] > 0) ptrObj->glDraw();
    glEnable(GL_CULL_FACE);
    glPopMatrix();
}

void Catoms3DGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int n) {
    glPushMatrix();
    mat.glMultMatrix();
    ptrObj->glDrawId(n);
    glPopMatrix();
}

void Catoms3DGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
    glPushMatrix();
    mat.glMultMatrix();
    ptrObj->glDrawIdByMaterial(n);
    glPopMatrix();
}

void Catoms3DGlBlock::fireSelectedTrigger() { 
    Lattice *lattice = World::getWorld()->lattice;
    const Cell3DPosition& bbPos = lattice->worldToGridPosition(getPosition());
    Catoms3DBlock* catom = static_cast<Catoms3DBlock*>(lattice->getBlock(bbPos));
 
    // custom user debug procedure
    if (catom and catom->blockCode) catom->blockCode->onBlockSelected();
}


}
