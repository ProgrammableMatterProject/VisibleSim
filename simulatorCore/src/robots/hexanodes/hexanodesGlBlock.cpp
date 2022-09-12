/**
 * @file   nodeGlBlock.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:05:42 2019
 *
 * @brief
 *
 *
 */

#include "hexanodesGlBlock.h"
#include "hexanodesWorld.h"
#include "../../grid/lattice.h"

namespace Hexanodes {

HexanodesGlBlock::HexanodesGlBlock(bID id):GlBlock(id), displayedValue(id) {
}

void HexanodesGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
    static GLint lx,ly;
#ifdef WIN32
    static GLint idTextureDigits = loadTexture((string(ROOT_DIR) + "/simulatorCore/resources/textures/smartBlocksTextures/digits.tga").c_str(),lx,ly);
#else
    static GLint idTextureDigits = loadTexture("../../simulatorCore/resources/textures/smartBlocksTextures/digits.tga",lx,ly);
#endif
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
// glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,c);
    } else {
// GLfloat gray[]={0.2,0.2,0.2,1.0};
        ptrObj->setLightedColor(color);
// glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,color);
    }
    ptrObj->glDraw();
    glPopMatrix(); // text is aligned along x axis
    if (displayedValue>=0) {
        glPushMatrix();

        int digits = 1;
        if (displayedValue>9) digits=2;
        if (displayedValue>99) digits=3;
        GLfloat dx = 20.0/digits;
        GLfloat x,s,t;
        int n=displayedValue;
        enableTexture(true);
        glBindTexture(GL_TEXTURE_2D,idTextureDigits);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
        GLfloat gray[]={0.2,0.2,0.2,1.0};
        glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,gray);

        glTranslatef(mat.m[3],mat.m[7],mat.m[11]+12.6);
        for (int i=0; i<digits; i++) {
            x = -11.0+(digits-i-1)*dx;
            s = ((n%10)%5)*0.2;
            t = 0.5-int((n%10)/5)*0.5;
            glNormal3f(0.0f,0.0f,1.0f);
            glBegin(GL_QUADS);
            glTexCoord2f(s,t);
            glVertex3f(x,-12.5f,0);
            glTexCoord2f(s+0.2,t);
            glVertex3f(x+dx,-12.5f,0);
            glTexCoord2f(s+0.2,t+0.5f);
            glVertex3f(x+dx,12.5f,0);
            glTexCoord2f(s,t+0.5f);
            glVertex3f(x,12.5f,0);
            glEnd();
            n/=10;
        }
        glDisable(GL_BLEND);

        glPopMatrix();
    }
}

void HexanodesGlBlock::glDrawShadows(ObjLoader::ObjLoader *ptrObj) {
    glPushMatrix();
    mat.glMultMatrix();
    ptrObj->glDraw();
    glPopMatrix();
}

void HexanodesGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int n) {
    glPushMatrix();
        mat.glMultMatrix();
        ptrObj->glDrawId(n);
    glPopMatrix();
}

void HexanodesGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
    glPushMatrix();
        mat.glMultMatrix();
    ptrObj->glDrawIdByMaterial(n);
    glPopMatrix();
}

}
