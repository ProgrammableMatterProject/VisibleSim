#include "okteenGlBlock.h"

using namespace BaseSimulator;

namespace Okteen {

OkteenGlBlock::OkteenGlBlock(bID id):GlBlock(id) {
    for (size_t i=0; i<6; i++) {
        tabPosConnectors[i]=0; // min length
    }
}

void OkteenGlBlock::setPosition(const Vector3D &pos) {
    position[0] = GLfloat(pos[0]);
    position[1] = GLfloat(pos[1]);
    position[2] = GLfloat(pos[2]);
    mat.setTranslation(pos);
}


void OkteenGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
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
    if (visible) ptrObj->glDraw();
    glPopMatrix();
}

void OkteenGlBlock::glDrawConnectors(ObjLoader::ObjLoader *ptrObj) {
    const float ampl = 1.47/255.0;
    glPushMatrix();
    mat.glMultMatrix();
    // Left connector
    glPushMatrix();
    glRotatef(-90.0f,0.0f,1.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Left]*ampl);
    ptrObj->glDraw();
    glPopMatrix();
    // Right connector
    glPushMatrix();
    glRotatef(90.0f,0.0f,1.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Right]*ampl);
    ptrObj->glDraw();
    glPopMatrix();
    // Front connector
    glPushMatrix();
    glRotatef(90.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Front]*ampl);
    ptrObj->glDraw();
    glPopMatrix();
    // Back connector
    glPushMatrix();
    glRotatef(-90.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Back]*ampl);
    ptrObj->glDraw();
    glPopMatrix();
    // Bottom connector
    glPushMatrix();
    glRotatef(180.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Bottom]*ampl);
    ptrObj->glDraw();
    glPopMatrix();
    // Top connector
    glPushMatrix();
    glTranslatef(0,0,tabPosConnectors[SCLattice::Top]*ampl);
    ptrObj->glDraw();
    glPopMatrix();

    glPopMatrix();
}

void OkteenGlBlock::glDrawIdConnectors(ObjLoader::ObjLoader *ptrObj,int n) {
    int m=n;
    const float ampl = 1.47/255.0;
    glPushMatrix();
    mat.glMultMatrix();
    // Left connector
    glPushMatrix();
    glRotatef(-90.0f,0.0f,1.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Left]*ampl);
    m=n;
    ptrObj->glDrawId(m);
    glPopMatrix();
    // Right connector
    glPushMatrix();
    glRotatef(90.0f,0.0f,1.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Right]*ampl);
    m=n;
    ptrObj->glDrawId(m);
    glPopMatrix();
    // Front connector
    glPushMatrix();
    glRotatef(90.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Front]*ampl);
    m=n;
    ptrObj->glDrawId(m);
    glPopMatrix();
    // Back connector
    glPushMatrix();
    glRotatef(-90.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Back]*ampl);
    m=n;
    ptrObj->glDrawId(m);
    glPopMatrix();
    // Bottom connector
    glPushMatrix();
    glRotatef(180.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Bottom]*ampl);
    m=n;
    ptrObj->glDrawId(m);
    glPopMatrix();
    // Top connector
    glPushMatrix();
    glTranslatef(0,0,tabPosConnectors[SCLattice::Top]*ampl);
    m=n;
    ptrObj->glDrawId(m);
    glPopMatrix();

    glPopMatrix();
}

void OkteenGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int n) {
    glPushMatrix();
    mat.glMultMatrix();
    ptrObj->glDrawId(n);
    glPopMatrix();
}

void OkteenGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
    const float ampl = 1.47/255.0;
    glPushMatrix();
    mat.glMultMatrix();
    // Bottom connector
    glPushMatrix();
    glRotatef(180.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Bottom]*ampl);
    ptrObj->glDrawId(n);
    glPopMatrix();
    // Back connector
    glPushMatrix();
    glRotatef(-90.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Back]*ampl);
    ptrObj->glDrawId(n);
    glPopMatrix();
    // Right connector
    glPushMatrix();
    glRotatef(90.0f,0.0f,1.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Right]*ampl);
    ptrObj->glDrawId(n);
    glPopMatrix();
    // Front connector
    glPushMatrix();
    glRotatef(90.0f,1.0f,0.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Front]*ampl);
    ptrObj->glDrawId(n);
    glPopMatrix();
    // Left connector
    glPushMatrix();
    glRotatef(-90.0f,0.0f,1.0f,0.0f);
    glTranslatef(0,0,tabPosConnectors[SCLattice::Left]*ampl);
    ptrObj->glDrawId(n);
    glPopMatrix();
    // Top connector
    glPushMatrix();
    glTranslatef(0,0,tabPosConnectors[SCLattice::Top]*ampl);
    ptrObj->glDrawId(n);
    glPopMatrix();

    glPopMatrix();
}

}
