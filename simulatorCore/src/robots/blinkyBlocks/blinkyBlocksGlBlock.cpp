#include "blinkyBlocksGlBlock.h"
#include <sstream>

namespace BlinkyBlocks {

    void BlinkyBlocksGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
        glPushMatrix();
        glTranslatef(position[0], position[1], position[2]);
        glRotatef(-90.0f * (float) (rotCoef), 0, 0, 1.0f);
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

    void BlinkyBlocksGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj, int n) {
        glPushMatrix();
        glTranslatef(position[0], position[1], position[2]);
        glRotatef(-90.0f * (float) (rotCoef), 0, 0, 1.0f);
        ptrObj->glDrawId(n);
        glPopMatrix();
    }

    void BlinkyBlocksGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) {
        glPushMatrix();
        glTranslatef(position[0], position[1], position[2]);
        glRotatef(-90.0f * (float) (rotCoef), 0, 0, 1.0f);
        ptrObj->glDrawIdByMaterial(n);
        glPopMatrix();
    }


    string BlinkyBlocksGlBlock::getInfo() const {
        ostringstream out;
        out << blockId << endl;
        out << fixed;
        out.precision(1);
        out << "Pos=(" << int(position[0] / 40) << "," << int(position[1] / 40) << "," << int(position[2] / 40) << ") ";
        out << "Col=(" << int(color[0]) << "," << int(color[1]) << "," << int(color[2]) << ")";
        return out.str();
    }

    string BlinkyBlocksGlBlock::getPopupInfo() const {
        ostringstream out;
        out << blockId << " -(" << int(position[0] / 40) << "," << int(position[1] / 40) << "," << int(position[2] / 40)
            << ") " << popupInfoString << "\n";
        return out.str();
    }

}
