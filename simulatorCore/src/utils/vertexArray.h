#ifndef VERTEXARRAY_H_
#define VERTEXARRAY_H_

#include <GL/glew.h>
#include <GL/freeglut.h>

class VertexArray {
	GLfloat *tabVertices,*tabNormals,*tabTexCoords;
	GLuint *tabIndices;
	GLuint nbrePts,nbreIndices;
public :
	VertexArray(int np,int ni);
	~VertexArray();

	void setPoint(int n,GLfloat px,GLfloat py,GLfloat pz,GLfloat nx,GLfloat ny,GLfloat nz,GLfloat tx,GLfloat ty);
	void setIndicesTriangle(int n,GLuint i1,GLuint i2,GLuint i3);
	void draw();
	void drawNormals();
};

#endif
