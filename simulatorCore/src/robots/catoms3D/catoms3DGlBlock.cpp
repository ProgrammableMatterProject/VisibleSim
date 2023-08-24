#include "catoms3DGlBlock.h"
#include "catoms3DBlock.h"
//#include "catoms3DWorld.h"

namespace Catoms3D {

    void Catoms3DGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
        glPushMatrix();
        mat.glMultMatrix();
        if (highlighted) {
            GLfloat n = 1.0 + 1.5 * (1.0 - (glutGet(GLUT_ELAPSED_TIME) % 1000) / 1000.0);
            GLfloat c[4];
            c[0] = color[0] * n;
            c[1] = color[1] * n;
            c[2] = color[2] * n;
            c[3] = 1.0;
            ptrObj->setLightedColor(c);
        } else {
            ptrObj->setLightedColor(color);
        }
        glDisable(GL_CULL_FACE);
        ptrObj->glDraw();
        glEnable(GL_CULL_FACE);
        glPopMatrix();
    }

    void Catoms3DGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj, int n) {
        glPushMatrix();
        mat.glMultMatrix();
        ptrObj->glDrawId(n);
        glPopMatrix();
    }

    void Catoms3DGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) {
        glPushMatrix();
        mat.glMultMatrix();
        ptrObj->glDrawIdByMaterial(n);
        glPopMatrix();
    }

    void Catoms3DGlBlock::fireSelectedTrigger() {
        /*Lattice *lattice = World::getWorld()->lattice;
        const Cell3DPosition& bbPos = lattice->worldToGridPosition(getPosition());
        Catoms3DBlock* catom = static_cast<Catoms3DBlock*>(lattice->getBlock(bbPos));

        // custom user debug procedure
        if (catom and catom->blockCode) catom->blockCode->onBlockSelected();*/
    }

    string Catoms3DGlBlock::getInfo() const {
        ostringstream out;
        out << blockId << endl;
        out << fixed;
        out.precision(1);
        Vector3D pos(mat[3], mat[7], mat[11]);
        int z = (int) ((pos[2] - 3) / 7.07);
        //out << "pp=(" << position[0] << ","<< position[1] << ","<< position[2] << ")  ";
        out << "Pos=(" << (int) ((pos[0] - 5.0 * (z % 2 == 1)) / 10) << ","
            << (int) ((pos[1] - 5.0 * (z % 2 == 1)) / 10) << "," << z << ") ";
        out << "Col=(" << (int) (color[0]) << "," << (int) (color[1]) << "," << (int) (color[2]) << ")";
        return out.str();
    }

/*string Catoms3DGlBlock::getPopupInfo() {
    string str="(" + to_string(int((mat[3]-5)/10)) + "," + to_string(int((mat[7]-5)/10)) + "," + to_string(int((mat[11]-3.5355)/7.07107)) + ")";
    return str;
}*/

    const Vector3D Catoms3DGlBlock::getPosition() const {
        return Vector3D(mat[3], mat[7], mat[11], 1);
    };

}