#include "glBlock.h"
#include "../gui/objLoader.h"
#include <sstream>

GlBlock::GlBlock(bID id) : blockId(id) {
    position[0] = 0.0;
    position[1] = 0.0;
    position[2] = 0.0;
    color[0] = 128;
    color[1] = 128;
    color[2] = 128;
    visible = true;
    highlighted = false;
}

GlBlock::GlBlock(bID id, const Vector3D &pos, const Vector3D &col) : blockId(id) {
    position[0] = pos[0];
    position[1] = pos[1];
    position[2] = pos[2];
    color[0] = GLubyte(col[0] * 255.0);
    color[1] = GLubyte(col[1] * 255.0);
    color[2] = GLubyte(col[2] * 255.0);
    visible = true;
    highlighted = false;
}

GlBlock::~GlBlock() {

}

void GlBlock::setPosition(const Vector3D &pos) {
    position[0] = GLfloat(pos[0]);
    position[1] = GLfloat(pos[1]);
    position[2] = GLfloat(pos[2]);
}

const Vector3D GlBlock::getPosition() const {
    return Vector3D(position[0],position[1],position[2],1);
}

void GlBlock::setColor(const Vector3D &col) {
    color[0] = GLubyte(col[0] * 255.0);
    color[1] = GLubyte(col[1] * 255.0);
    color[2] = GLubyte(col[2] * 255.0);
}

void GlBlock::setColor(const Color &col) {
    color[0] = col[0];
    color[1] = col[1];
    color[2] = col[2];
}

bool GlBlock::isVisible() const {
    return visible;
}

void GlBlock::setVisible(bool v) {
    visible = v;
}

void GlBlock::toggleHighlight() {
    highlighted = !highlighted;
}

string GlBlock::getInfo() const {
    ostringstream out;
    out << blockId << endl;
    out << fixed;
    out.precision(1);
    out << "Pos=(" << position[0] << "," << position[1] << "," << position[2] << ") ";
    out << "Col=(" << (int) (color[0]) << "," << (int) (color[1]) << "," << (int) (color[2]) << ")";
    return out.str();
}

string GlBlock::getPopupInfo() const {
    ostringstream out;
    out << "#" << blockId << ": (" << position[0] << "," << position[1] << "," << position[2] << ")";
    if (popupInfoString != "") { out << "(" << popupInfoString << ")"; }
    out << "\n";
    return out.str();
}

void GlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj, int n) {
    glPushMatrix();
    glTranslatef(position[0], position[1], position[2]);
    ptrObj->glDrawId(n);
    glPopMatrix();
}

void GlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) {
    glPushMatrix();
    glTranslatef(position[0], position[1], position[2]);
    ptrObj->glDrawIdByMaterial(n);
    glPopMatrix();
}

