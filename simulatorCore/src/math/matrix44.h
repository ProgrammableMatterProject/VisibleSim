/*
 * matrix44.h
 *
 *  Created on: 29 janv. 2012
 *
 */

#ifndef matrix44_H_
#define matrix44_H_

#include <GL/glew.h>
#include <iostream>
#include <fstream>
#include <math.h>
#include "math/vector3D.h"
#if !defined(M_PI)
#define M_PI	3.1415926535897932384626433832795
#endif

using namespace std;

class Matrix
{ public :
  double m[16];

  Matrix() { identity(); };
  Matrix(const float *);
  void identity() { memset(m,0,16*sizeof(double)); m[0]=m[5]=m[10]=m[15]=1.; };
  friend istream& operator>>(istream& f,Matrix &p);
  friend ostream& operator<<(ostream& f,Matrix &p);
  friend const Matrix operator *(const Matrix,const Matrix);
  friend const Vector3D operator *(const Matrix,const Vector3D);
  inline double operator[](const int i) { return m[i]; };
  inline void setTranslation(Vector3D V) { identity(); m[3]=V[0]; m[7]=V[1]; m[11]=V[2]; m[15]=1.0; };
  inline void setTranslation(double Vx,double Vy,double Vz) { identity(); m[3]=Vx; m[7]=Vy; m[11]=Vz; m[15]=1.0; };
  inline void setTranslationGL(Vector3D V) { identity(); m[12]=V[0]; m[13]=V[1]; m[14]=V[2]; m[15]=1.0; };
  inline void setHomothetie(double hx,double hy,double hz) { identity(); m[0]=hx; m[5]=hy; m[10]=hz; m[15]=1.0; };
  void setRotationX(double a);
  void setRotationY(double a);
  void setRotationZ(double a);
  void setRotation(double a,const Vector3D &V);
  double determinant() const;
  void inverse(Matrix &) const;
  void transpose(Matrix &) const;
  void setGLmat(const Matrix &,const Vector3D &);
  void set(double,double,double,double,double,double,double,double,double,double,double,double,double,double,double,double);
  void setFromGL(GLfloat *mat);
  void glLoadMatrix();
  void glMultMatrix();
  void fillArray(GLdouble *);
  void fillArray(GLfloat *);
};

const Matrix operator *(const Matrix,const Matrix);
const Vector3D operator *(const Matrix,const Vector3D);
const Vector3D operator *(const Vector3D,const Matrix);

double det33(double,double,double,double,double,double,double,double,double);

#endif /* matrix44_H_ */
