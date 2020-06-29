#include "glBlock.h"
#include "../gui/objLoader.h"

#include <sstream>

GlBlock::GlBlock(bID id):blockId(id) {
    position[0] = 0.0;
    position[1] = 0.0;
    position[2] = 0.0;
    color[0] = 128;
    color[1] = 128;
    color[2] = 128;
    visible = true;
    isHighlighted = false;
}

GlBlock::GlBlock(bID id,const Vector3D &pos, const Vector3D &col) : blockId(id) {
    position[0] = pos[0];
    position[1] = pos[1];
    position[2] = pos[2];
    color[0] = GLubyte(col[0]*255.0);
    color[1] = GLubyte(col[1]*255.0);
    color[2] = GLubyte(col[2]*255.0);
    visible = true;
    isHighlighted = false;
}

GlBlock::~GlBlock() {

}

void GlBlock::setPosition(const Vector3D &pos) {
    position[0] = GLfloat(pos[0]);
    position[1] = GLfloat(pos[1]);
    position[2] = GLfloat(pos[2]);
}

void GlBlock::setColor(const Vector3D &col) {
    color[0] = GLubyte(col[0]*255.0);
    color[1] = GLubyte(col[1]*255.0);
    color[2] = GLubyte(col[2]*255.0);
}

void GlBlock::setColor(const Color &col) {
    color[0] = GLubyte(col[0]*255.0);
    color[1] = GLubyte(col[1]*255.0);
    color[2] = GLubyte(col[2]*255.0);
}

bool GlBlock::isVisible() {
    return visible;
}

void GlBlock::setVisible(bool v) {
    visible=v;
}

void GlBlock::toggleHighlight() {
    isHighlighted=!isHighlighted;
}

//using namespace Catoms3D; //FIXME:
string GlBlock::getInfo() {
    ostringstream out;
    out << blockId << endl;
    out << fixed;
    out.precision(1);
    out << "Pos=(" << position[0] << "," << position[1] << "," << position[2] << ") ";
    out << "Col=(" << (int)(color[0] * 255) << "," << (int)(color[1] * 255) << "," << (int)(color[2] * 255) << ")";

    return out.str();
}
/*
string GlBlock::getPopupInfo() {
    ostringstream out;
    out << blockId << " - "
        << World::getWorld()->lattice->worldToGridPosition(getPosition()) <<"\n";

    return out.str();
}
*/
void GlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int n) {
    glPushMatrix();
    glTranslatef(position[0],position[1],position[2]);
		ptrObj->glDrawId(n);
    glPopMatrix();
}

void GlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
    glPushMatrix();
    glTranslatef(position[0],position[1],position[2]);
        ptrObj->glDrawIdByMaterial(n);
    glPopMatrix();
}

