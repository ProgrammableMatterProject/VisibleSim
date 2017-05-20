#include "oktenGlBlock.h"
#include "lattice.h"

namespace Okten {

OktenGlBlock::OktenGlBlock(bID id):GlBlock(id) {
    for (size_t i=0; i<6; i++) {
        tabPosConnectors[i]=0; // min length
    }
}

void OktenGlBlock::glDraw(ObjLoader::ObjLoader *ptrObj) {
	glPushMatrix();
	mat.glMultMatrix();
	/*glTranslatef(position[0],position[1],position[2]);
	glRotatef(psi,1,0,0);
	glRotatef(phi,0,1,0);
	glRotatef(theta,0,0,1);*/
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
	glDisable(GL_CULL_FACE);
	if (color[3] > 0) ptrObj->glDraw();
	glEnable(GL_CULL_FACE);
    glPopMatrix();
}

void OktenGlBlock::glDrawConnectors(ObjLoader::ObjLoader *ptrObj) {
    const float ampl = 1.47/255.0;
	glPushMatrix();
	mat.glMultMatrix();
	// Left connector
	glPushMatrix();
	glRotatef(-90.0f,0.0f,1.0f,0.0f);
	glTranslatef(0,0,tabPosConnectors[SCLattice::Left]*ampl);
	ptrObj->glDraw();
	glPopMatrix();
	// Right connector
	glPushMatrix();
	glRotatef(90.0f,0.0f,1.0f,0.0f);
	glTranslatef(0,0,tabPosConnectors[SCLattice::Right]*ampl);
	ptrObj->glDraw();
	glPopMatrix();
	// Front connector
	glPushMatrix();
	glRotatef(90.0f,1.0f,0.0f,0.0f);
	glTranslatef(0,0,tabPosConnectors[SCLattice::Front]*ampl);
	ptrObj->glDraw();
	glPopMatrix();
	// Back connector
	glPushMatrix();
	glRotatef(-90.0f,1.0f,0.0f,0.0f);
	glTranslatef(0,0,tabPosConnectors[SCLattice::Back]*ampl);
	ptrObj->glDraw();
	glPopMatrix();

	// Bottom connector
	glPushMatrix();
	glRotatef(180.0f,1.0f,0.0f,0.0f);
	glTranslatef(0,0,tabPosConnectors[SCLattice::Bottom]*ampl);
	ptrObj->glDraw();
	glPopMatrix();
	// Top connector
	glPushMatrix();
	glTranslatef(0,0,tabPosConnectors[SCLattice::Top]*ampl);
	ptrObj->glDraw();
	glPopMatrix();

	glPopMatrix();
}

}
