#include "catoms3DGlBlock.h"

namespace Catoms3D {

Catoms3DGlBlock::Catoms3DGlBlock(int id) : GlBlock(id) {
    angle=0;
}

Catoms3DGlBlock::~Catoms3DGlBlock() {
}

void Catoms3DGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
	glPushMatrix();
	glTranslatef(position[0],position[1],position[2]);
	//glRotatef(-angle,0,1,0);
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

void Catoms3DGlBlock::glDrawId(ObjLoader::ObjLoader *ptrObj,int &n) {
	glPushMatrix();
	glTranslatef(position[0],position[1],position[2]);
	ptrObj->glDrawId(n);
	glPopMatrix();
}

void Catoms3DGlBlock::glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) {
	glPushMatrix();
	glTranslatef(position[0],position[1],position[2]);
	ptrObj->glDrawIdByMaterial(n);
	glPopMatrix();
}


string Catoms3DGlBlock::getInfo() {
    char tmp[1024];
	sprintf(tmp,"%d\nPos=(%.1f,%.1f,%.1f) Col=(%4.1f,%4.1f,%4.1f)",blockId,position[0],position[1],position[2],color[0],color[1],color[2]);

	return string(tmp);
}

}
