#include "slidingCubesGlBlock.h"

namespace SlidingCubes {

    void SlidingCubesGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
        glPushMatrix();
        mat.glMultMatrix();
        if (highlighted) {
            GLfloat n = (0.5 + 1.5 * (1.0 - (glutGet(GLUT_ELAPSED_TIME) % 1000) / 1000.0));
            float c[4];
            c[0] = color[0] * n;
            c[1] = color[1] * n;
            c[2] = color[2] * n;
            c[3] = 1.0;
            ptrObj->setLightedColor(c);
        } else {
            ptrObj->setLightedColor(color);
        }
        ptrObj->glDraw();
        glPopMatrix();
    }

    void SlidingCubesGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj, int n) {
        glPushMatrix();
        mat.glMultMatrix();
        ptrObj->glDrawId(n);
        glPopMatrix();
    }

    void SlidingCubesGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
        glPushMatrix();
        mat.glMultMatrix();
        ptrObj->glDrawIdByMaterial(n);
        glPopMatrix();
    }

    string SlidingCubesGlBlock::getInfo() const {
        ostringstream out;
        out << blockId << endl;
        out << fixed;
        out.precision(1);
        //out << "pp=(" << position[0] << ","<< position[1] << ","<< position[2] << ")  ";
        out << "Pos=(" << int(position[0] / 10) << "," << int(position[1] / 10) << "," << int(position[2] / 10) << ") ";
        out << "Col=(" << int(color[0]) << "," << int(color[1]) << "," << int(color[2]) << ")";
        return out.str();
    }

    string SlidingCubesGlBlock::getPopupInfo() const {
        ostringstream out;
        out << blockId << " -(" << int(position[0] / 10) << "," << int(position[1] / 10) << "," << int(position[2] / 10)
            << ") " << popupInfoString << "\n";
        return out.str();
    }

}
