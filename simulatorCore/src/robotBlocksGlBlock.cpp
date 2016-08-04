#include "robotBlocksGlBlock.h"

namespace RobotBlocks {

RobotBlocksGlBlock::RobotBlocksGlBlock(bID id) : GlBlock(id) {
}

RobotBlocksGlBlock::~RobotBlocksGlBlock() {
}

void RobotBlocksGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
	glPushMatrix();

	glTranslatef(position[0],position[1],position[2]);
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
	ptrObj->glDraw();
    glPopMatrix();
}

void RobotBlocksGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int &n) {
	glPushMatrix();
	glTranslatef(position[0],position[1],position[2]);
	ptrObj->glDrawId(n);
	glPopMatrix();
}

void RobotBlocksGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
	glPushMatrix();
	glTranslatef(position[0],position[1],position[2]);
	ptrObj->glDrawIdByMaterial(n);
	glPopMatrix();
}


string RobotBlocksGlBlock::getInfo() {
    char tmp[1024];
	sprintf(tmp,"%llu\nPos=(%.1f,%.1f,%.1f) Col=(%4.1f,%4.1f,%4.1f)\n",blockId,position[0],position[1],position[2],color[0],color[1],color[2]);

	return string(tmp);
}

void RobotBlocksGlBlock::setPrevNext(int p,int n) {
	prevId = p;
	nextId = n;
}

}
