#include "smartBlocksGlBlock.h"

namespace SmartBlocks {

void SmartBlocksGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
    static GLint lx,ly;
#ifdef WIN32
    static GLint idTextureDigits = loadTexture((string(ROOT_DIR) + "/simulatorCore/resources/textures/smartBlocksTextures/digits.tga").c_str(),lx,ly);
#else
    static GLint idTextureDigits = loadTexture("../../simulatorCore/resources/textures/smartBlocksTextures/digits.tga",lx,ly);
#endif

    glPushMatrix();
    glTranslatef(position[0]+12.5,position[1]+12.5,position[2]);
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
    ptrObj->glDraw();

    if (displayedValue<noDisplay) {
        int digits = 2;
        if (displayedValue>9) digits=2;
        if (displayedValue>99) digits=3;
        GLfloat dx = 20.0/digits;
        GLfloat x,s,t;
        int n=displayedValue;
        glBindTexture(GL_TEXTURE_2D,idTextureDigits);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
        for (int i=0; i<digits; i++) {
            x = -11.0+(digits-i-1)*dx;
            s = ((n%10)%5)*0.2;
            t = 0.5-int((n%10)/5)*0.5;
            glNormal3f(0.0f,0.0f,1.0f);
            glBegin(GL_QUADS);
            glTexCoord2f(s,t);
            glVertex3f(x,-12.5f,12.6f);
            glTexCoord2f(s+0.2,t);
            glVertex3f(x+dx,-12.5f,12.6f);
            glTexCoord2f(s+0.2,t+0.5f);
            glVertex3f(x+dx,12.5f,12.6f);
            glTexCoord2f(s,t+0.5f);
            glVertex3f(x,12.5f,12.6f);
            glEnd();
            n/=10;
        }
        glDisable(GL_BLEND);
    }
    glPopMatrix();
}

string SmartBlocksGlBlock::getPopupInfo() const {
    string out=to_string(blockId) + " - (" + to_string((int)(position[0]/25.0f)) +
            "," + to_string((int)(position[1]/25.0f)) + ")\n";
    return out;
}

void SmartBlocksGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int n) {
    glPushMatrix();
    glTranslatef(position[0]+12.5,position[1]+12.5,position[2]);
    ptrObj->glDrawId(n);
    glPopMatrix();
}

void SmartBlocksGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
    glPushMatrix();
    glTranslatef(position[0]+12.5,position[1]+12.5,position[2]);
    ptrObj->glDrawIdByMaterial(n);
    glPopMatrix();
}

}
