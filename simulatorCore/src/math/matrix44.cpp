/*
 * matrix44.cpp
 *
 *  Created on: 29 janv. 2012
 *
 */

#include "matrix44.h"

Matrix::Matrix(const float *tab)
{ int i=16;
  float *ptr=m;
  while (i--)
  { *ptr++ = *tab++;
  }
}

// lecture d'un Matrix dans un flux
istream& operator>>(istream& f,Matrix&p)
{ for (int i=0; i<16; i++)
  { f >> p.m[i];
  }
  return f;
}

// �criture d'un Matrix dans un flux
ostream& operator<<(ostream& f,const Matrix&p)
{ f << "|" << p.m[0] << "," << p.m[1] << "," << p.m[2] << "," << p.m[3] << "|" << endl;
  f << "|" << p.m[4] << "," << p.m[5] << "," << p.m[6] << "," << p.m[7] << "|" << endl;
  f << "|" << p.m[8] << "," << p.m[9] << "," << p.m[10] << "," << p.m[11] << "|" << endl;
  f << "|" << p.m[12] << "," << p.m[13] << "," << p.m[14] << "," << p.m[15] << "|" << endl;
  return f;
}

const Vector3D operator *(const Matrix p1, const Vector3D p2)
{ float x=0,y=0,z=0,w=0;
  for (int i=0; i<4; i++)
  { x += p1.m[i]*p2[i];
    y += p1.m[i+4]*p2[i];
    z += p1.m[i+8]*p2[i];
    w += p1.m[i+12]*p2[i];
  }
  return Vector3D(x,y,z,w);
}

const Vector3D operator *(const Vector3D p1, const Matrix p2)
{ float x,y,z,w;
  for (int i=0; i<4; i++)
  { x += p2.m[i*4]*p1[i];
    y += p2.m[i*4+1]*p1[i];
    z += p2.m[i*4+2]*p1[i];
    w += p2.m[i*4+3]*p1[i];
  }
  return Vector3D(x,y,z,w);
}

const Matrix operator *(const Matrix p1, const Matrix p2)
{ Matrix r;
  int l,c,i;

  for (l=0; l<4; l++)
  { for (c=0; c<4; c++)
    { r.m[l*4+c]=0;
      for (i=0; i<4; i++)
      { r.m[l*4+c]+=p1.m[l*4+i]*p2.m[i*4+c];
      }
    }
  }
  return r;
}

void Matrix::setRotationX(float a)
{ identity();
  float cs=cos(a*M_PI/180.),sn=sin(a*M_PI/180.);
  m[5]=m[10]=cs; m[6]=-sn; m[9]=sn;
}

void Matrix::setRotationY(float a)
{ identity();
  float cs=cos(a*M_PI/180.),sn=sin(a*M_PI/180.);
  m[0]=m[10]=cs; m[2]=sn; m[8]=-sn;
}

void Matrix::setRotationZ(float a)
{ identity();
  float cs=cos(a*M_PI/180.),sn=sin(a*M_PI/180.);
  m[0]=m[5]=cs; m[1]=-sn; m[4]=sn;
}

double det33(double a,double b,double c,double d,double e,double f,double g,double h,double i)
{ return a*(e*i-h*f)-d*(b*i-h*c)+g*(b*f-e*c);
}

double Matrix::determinant() const
{ return ( m[0] * det33(m[5],m[6],m[7],m[9],m[10],m[11],m[13],m[14],m[15])-
           m[4] * det33(m[1],m[2],m[3],m[9],m[10],m[11],m[13],m[14],m[15])+
           m[8] * det33(m[1],m[2],m[3],m[5],m[6],m[7],m[13],m[14],m[15])-
           m[12]* det33(m[1],m[2],m[3],m[5],m[6],m[7],m[9],m[10],m[11]));
}


/**
    * \brief calculate the inverse of the current matrix
    \param result of the invertion of the current matrix
*/
void Matrix::inverse(Matrix &inv) const
{ double det = determinant();
  inv.m[0] = det33(m[5],m[6],m[7],m[9],m[10],m[11],m[13],m[14],m[15])/det;
  inv.m[4] = -det33(m[4],m[6],m[7],m[8],m[10],m[11],m[12],m[14],m[15])/det;
  inv.m[8] = det33(m[4],m[5],m[7],m[8],m[9],m[11],m[12],m[13],m[15])/det;
  inv.m[12] = -det33(m[4],m[5],m[6],m[8],m[9],m[10],m[12],m[13],m[14])/det;

  inv.m[1] = -det33(m[1],m[2],m[3],m[9],m[10],m[11],m[13],m[14],m[15])/det;
  inv.m[5] = det33(m[0],m[2],m[3],m[8],m[10],m[11],m[12],m[14],m[15])/det;
  inv.m[9] = -det33(m[0],m[1],m[3],m[8],m[9],m[11],m[12],m[13],m[15])/det;
  inv.m[13] = det33(m[0],m[1],m[2],m[8],m[9],m[10],m[12],m[13],m[14])/det;

  inv.m[2] = det33(m[1],m[2],m[3],m[5],m[6],m[7],m[13],m[14],m[15])/det;
  inv.m[6] = -det33(m[0],m[2],m[3],m[4],m[6],m[7],m[12],m[14],m[15])/det;
  inv.m[10] = det33(m[0],m[1],m[3],m[4],m[5],m[7],m[12],m[13],m[15])/det;
  inv.m[14] = -det33(m[0],m[1],m[2],m[4],m[5],m[6],m[12],m[13],m[14])/det;

  inv.m[3] = -det33(m[1],m[2],m[3],m[5],m[6],m[7],m[9],m[10],m[11])/det;
  inv.m[7] = det33(m[0],m[2],m[3],m[4],m[6],m[7],m[8],m[10],m[11])/det;
  inv.m[11] = -det33(m[0],m[1],m[3],m[4],m[5],m[7],m[8],m[9],m[11])/det;
  inv.m[15] = det33(m[0],m[1],m[2],m[4],m[5],m[6],m[8],m[9],m[10])/det;
}

void Matrix::transpose(Matrix &t) const
{ t.m[0] = m[0];
  t.m[1] = m[4];
  t.m[2] = m[8];
  t.m[3] = m[12];
  t.m[4] = m[1];
  t.m[5] = m[5];
  t.m[6] = m[9];
  t.m[7] = m[13];
  t.m[8] = m[2];
  t.m[9] = m[6];
  t.m[10] = m[10];
  t.m[11] = m[14];
  t.m[12] = m[3];
  t.m[13] = m[7];
  t.m[14] = m[11];
  t.m[15] = m[15];
}

void Matrix::set(float x00,float x10,float x20,float x30,
                  float x01,float x11,float x21,float x31,
                  float x02,float x12,float x22,float x32,
                  float x03,float x13,float x23,float x33)
{ m[0] = x00;
  m[1] = x10;
  m[2] = x20;
  m[3] = x30;
  m[4] = x01;
  m[5] = x11;
  m[6] = x21;
  m[7] = x31;
  m[8] = x02;
  m[9] = x12;
  m[10] = x22;
  m[11] = x32;
  m[12] = x03;
  m[13] = x13;
  m[14] = x23;
  m[15] = x33;
}

void Matrix::setGLmat(const Matrix &R,const Vector3D &T)
{ m[0] = R.m[0];
  m[1] = R.m[4];
  m[2] = R.m[8];
  m[3] = R.m[12];
  m[4] = R.m[1];
  m[5] = R.m[5];
  m[6] = R.m[9];
  m[7] = R.m[13];
  m[8] = R.m[2];
  m[9] = R.m[6];
  m[10] = R.m[10];
  m[11] = R.m[14];
  m[12] = T[0];
  m[13] = T[1];
  m[14] = T[2];
  m[15] = 1.0;

}

void Matrix::setRotation(float a,const Vector3D &V)
{ double cosa=cos(a*M_PI/180.),sina=sin(a*M_PI/180.),c1=1.0-cosa;

  m[0] = V[0]*V[0]*c1 + cosa;
  m[1] = V[0]*V[1]*c1 - V[2]*sina;
  m[2] = V[0]*V[2]*c1 + V[1]*sina;
  m[3] = 0.0;
  m[4] = V[0]*V[1]*c1 + V[2]*sina;
  m[5] = V[1]*V[1]*c1 + cosa;
  m[6] = V[1]*V[2]*c1 - V[0]*sina;
  m[7] = 0.0;
  m[8] = V[0]*V[2]*c1 - V[1]*sina;
  m[9] = V[1]*V[2]*c1 + V[0]*sina;
  m[10] = V[2]*V[2]*c1 + cosa;
  m[11] = 0.0;
  m[12] = 0.0;
  m[13] = 0.0;
  m[14] = 0.0;
  m[15] = 1.0;
}

void Matrix::setFromGL(GLfloat *mat) {
    // copie en transposant !
    m[0] = mat[0];
    m[1] = mat[4];
    m[2] = mat[8];
    m[3] = mat[12];
    m[4] = mat[1];
    m[5] = mat[5];
    m[6] = mat[9];
    m[7] = mat[13];
    m[8] = mat[2];
    m[9] = mat[6];
    m[10] = mat[10];
    m[11] = mat[14];
    m[12] = mat[3];
    m[13] = mat[7];
    m[14] = mat[11];
    m[15] = mat[15];
}

void Matrix::glLoadMatrix() {
    GLfloat mat[16];

    mat[0] = GLfloat(m[0]);
    mat[1] = GLfloat(m[4]);
    mat[2] = GLfloat(m[8]);
    mat[3] = GLfloat(m[12]);
    mat[4] = GLfloat(m[1]);
    mat[5] = GLfloat(m[5]);
    mat[6] = GLfloat(m[9]);
    mat[7] = GLfloat(m[13]);
    mat[8] = GLfloat(m[2]);
    mat[9] = GLfloat(m[6]);
    mat[10] = GLfloat(m[10]);
    mat[11] = GLfloat(m[14]);
    mat[12] = GLfloat(m[3]);
    mat[13] = GLfloat(m[7]);
    mat[14] = GLfloat(m[11]);
    mat[15] = GLfloat(m[15]);
    glLoadMatrixf(mat);
}

void Matrix::glMultMatrix() {
    GLfloat mat[16];

    mat[0] = GLfloat(m[0]);
    mat[1] = GLfloat(m[4]);
    mat[2] = GLfloat(m[8]);
    mat[3] = GLfloat(m[12]);
    mat[4] = GLfloat(m[1]);
    mat[5] = GLfloat(m[5]);
    mat[6] = GLfloat(m[9]);
    mat[7] = GLfloat(m[13]);
    mat[8] = GLfloat(m[2]);
    mat[9] = GLfloat(m[6]);
    mat[10] = GLfloat(m[10]);
    mat[11] = GLfloat(m[14]);
    mat[12] = GLfloat(m[3]);
    mat[13] = GLfloat(m[7]);
    mat[14] = GLfloat(m[11]);
    mat[15] = GLfloat(m[15]);
    glMultMatrixf(mat);
}

void Matrix::fillArray(GLdouble *mat) {
    memcpy(mat,m,16*sizeof(GLdouble));
}

void Matrix::fillArray(GLfloat *mat) {
    int i;
    for (i=0; i<16; i++) { mat[i]=GLfloat(m[i]); };
}
