#include "glBlock.h"
#include "world.h"

#include <sstream>

#include "objLoader.h"

GlBlock::GlBlock(bID id):blockId(id) {
	position[0] = 0.0;
	position[1] = 0.0;
	position[2] = 0.0;
	color[0] = 0.5;
	color[1] = 0.5;
	color[2] = 0.5;
	color[3] = 1.0;
	isHighlighted = false;
}

GlBlock::GlBlock(bID id,const Vector3D &pos, const Vector3D &col) : blockId(id) {
	position[0] = pos[0];
	position[1] = pos[1];
	position[2] = pos[2];
	color[0] = col[0];
	color[1] = col[1];
	color[2] = col[2];
	color[3] = 1.0;
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
	color[0] = GLfloat(col[0]);
	color[1] = GLfloat(col[1]);
	color[2] = GLfloat(col[2]);
	color[3] = 1.0;
}

void GlBlock::setColor(const Color &col) {
	color[0] = col[0];
	color[1] = col[1];
	color[2] = col[2];
	color[3] = 1.0;
}

void GlBlock::setVisible(bool visible) {
    color[3] = visible;
}

void GlBlock::toggleHighlight() {
	isHighlighted=!isHighlighted;
}

string GlBlock::getInfo() {
    ostringstream out;
	out << blockId << "\n";
	out << fixed;
	out.precision(1);
	out << "Pos=(" << position[0] << "," << position[1] << "," << position[2] << ") ";
	out << "Col=(" << (int)(color[0] * 255) << "," << (int)(color[1] * 255) << "," << (int)(color[2] * 255) << ")";

	return out.str();
}

string GlBlock::getPopupInfo() {
    ostringstream out;
	out << blockId << " - " << World::getWorld()->lattice->worldToGridPosition(getPosition()) <<"\n";

	return out.str();
}


void GlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int &n) {
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
