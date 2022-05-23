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
#include "vector3D.h"
#if !defined(M_PI)
#define M_PI	3.1415926535897932384626433832795
#endif

using namespace std;

class Matrix {
public :
  float m[16];

  Matrix() { identity(); };
  Matrix(float m00,float m10,float m20,float m30,float m01,float m11,float m21,float m31,
         float m02,float m12,float m22,float m32,float m03,float m13,float m23,float m33) {
      set(m00,m10,m20,m30,m01,m11,m21,m31,m02,m12,m22,m32,m03,m13,m23,m33);
  }
  Matrix(const float *);
  void identity() { memset(m,0,16*sizeof(float)); m[0]=m[5]=m[10]=m[15]=1.; };
  friend istream& operator>>(istream& f,Matrix &p);
  friend ostream& operator<<(ostream& f,const Matrix &p);
  friend const Matrix operator *(const Matrix,const Matrix);
  friend const Vector3D operator *(const Matrix,const Vector3D);
  inline  double operator[](const int i) const { return m[i]; };
  inline void setTranslation(Vector3D V) { identity(); m[3]=V[0]; m[7]=V[1]; m[11]=V[2]; m[15]=1.0; };
  inline void setTranslation(float Vx,float Vy,float Vz) { identity(); m[3]=Vx; m[7]=Vy; m[11]=Vz; m[15]=1.0; };
  inline void setTranslationGL(Vector3D V) { identity(); m[12]=V[0]; m[13]=V[1]; m[14]=V[2]; m[15]=1.0; };
  inline void setHomothetie(float hx,float hy,float hz) { identity(); m[0]=hx; m[5]=hy; m[10]=hz; m[15]=1.0; };
  void setRotationX(float a);
  void setRotationY(float a);
  void setRotationZ(float a);
  void setRotation(float a,const Vector3D &V);
  double determinant() const;
  void inverse(Matrix &) const;
  void transpose(Matrix &) const;
  void setGLmat(const Matrix &,const Vector3D &);
  void set(float,float,float,float,float,float,float,float,float,float,float,float,float,float,float,float);
  void setFromGL(GLfloat *mat);
  void glLoadMatrix();
  void glMultMatrix();
  void fillArray(GLdouble *);
  void fillArray(GLfloat *);
  Vector3D getPosition() const { return Vector3D(m[3],m[7],m[11],1.0); }
};

const Matrix operator *(const Matrix,const Matrix);
const Vector3D operator *(const Matrix,const Vector3D);
const Vector3D operator *(const Vector3D,const Matrix);

double det33(double,double,double,double,double,double,double,double,double);

#endif /* matrix44_H_ */
