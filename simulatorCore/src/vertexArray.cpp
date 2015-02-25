#include "vertexArray.h"
#include "stdio.h"
VertexArray::VertexArray(int np,int nt) {
	nbrePts=np;
	nbreIndices=nt*3;
	tabVertices = new GLfloat[nbrePts*3];
	tabNormals = new GLfloat[nbrePts*3];
	tabTexCoords = new GLfloat[nbrePts*2];
	tabIndices = new GLuint[nbreIndices];
}

VertexArray::~VertexArray() {
	delete [] tabVertices;
	delete [] tabNormals;
	delete [] tabTexCoords;
	delete [] tabIndices;
}

void VertexArray::setPoint(int n,GLfloat px,GLfloat py,GLfloat pz,GLfloat nx,GLfloat ny,GLfloat nz,GLfloat tx,GLfloat ty) {
	GLfloat *ptr=tabVertices+n*3;
	*ptr++=px;
	*ptr++=py;
	*ptr=pz;
	ptr = tabNormals+n*3;
	*ptr++=nx;
	*ptr++=ny;
	*ptr=nz;
	ptr = tabTexCoords+n*2;
	*ptr++=tx;
	*ptr=ty;
}

void VertexArray::setIndicesTriangle(int n,GLuint i1,GLuint i2,GLuint i3) {
	GLuint *ptr=tabIndices+n*3;
	*ptr++=i1;
	*ptr++=i2;
	*ptr=i3;
}

void VertexArray::draw() {
	glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnableClientState(GL_VERTEX_ARRAY);
    glNormalPointer(GL_FLOAT, 0, tabNormals);
    glTexCoordPointer(2, GL_FLOAT, 0, tabTexCoords);
	glVertexPointer(3, GL_FLOAT, 0, tabVertices);
	glDrawElements(GL_TRIANGLES, nbreIndices,GL_UNSIGNED_INT,tabIndices);
	glDisableClientState(GL_VERTEX_ARRAY);  // disable vertex arrays
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
}

void VertexArray::drawNormals() {
	glBegin(GL_LINES);
	int i = nbrePts;
	GLfloat *ptrPts = tabVertices;
	GLfloat *ptrNorm = tabNormals;
	while (i--) {
		glVertex3fv(ptrPts);
		glVertex3f(ptrPts[0]+ptrNorm[0]*10.0,ptrPts[1]+ptrNorm[1]*10.0,ptrPts[2]+ptrNorm[2]*10.0);
		ptrPts+=3;
		ptrNorm+=3;
	}
	glEnd();
}
