/*!
 * \file vector3D.h
 * \brief 3D homogeneous vector
 * \date 29/01/2012
 * \author Benoît Piranda
 */

#ifndef VECTOR3D_H_
#define VECTOR3D_H_

#include <iostream>
#include <fstream>
#include <cmath>
#ifndef _WIN
#include <memory.h>
#endif

using namespace std;

/**
 * \class Vector3D vector3D.h
*/
class Vector3D
{ public :
  double pt[4]; //!< x,y,z,w in a table (w=0 for a vector, w=1 for a point)

/**
    \brief Constructor, initialize the vector to 0
*/
  Vector3D() { memset(pt,0,4*sizeof(double)); };
/**
    \brief Constructor, initialize the vector to (x,y,z,w)
    \param x : x coordinate of the vector
    \param y : y coordinate of the vector
    \param z : z coordinate of the vector
    \param w : w=0 for a vector (default), w=1 for a point
*/
  Vector3D(double x,double y,double z,double w=0.0) { pt[0]=x; pt[1]=y; pt[2]=z; pt[3]=w; };
/**
    \brief Set method, initialize the vector to (x,y,z,w)
    \param x : x coordinate of the vector
    \param y : y coordinate of the vector
    \param z : z coordinate of the vector
    \param w : w=0 for a vector (default), w=1 for a point
*/
  inline void set(double x,double y,double z,double w=0.0) { pt[0]=x; pt[1]=y; pt[2]=z; pt[3]=w; };
/**
    \brief Set method, initialize the vector from an array of float
    \param tab : tab of coordinates
    \param s : size of tab
*/
  void set(const float *tab,short s,float extra=0.0f);
  void setMin(double x,double y,double z) { if (x<pt[0]) pt[0]=x; if (y<pt[1]) pt[1]=y; if (z<pt[2]) pt[2]=z; };
  void setMax(double x,double y,double z) { if (x>pt[0]) pt[0]=x; if (y>pt[1]) pt[1]=y; if (z>pt[2]) pt[2]=z; };
/**
    \brief Return a normalized copy of the vector
*/
  const Vector3D normer() const;
/**
    \brief Normalize the current vector
*/
  void normer_interne();
/**
    \brief Normalize the current vector with a lenght l
    \param l : length of the vector
*/
  void setLength(double l);
/**
    \brief Return the length of the vector
*/
  double norme() const;
/**
    \brief Return the square of the length of the vector
*/
  double norme2() const;
/**
    \brief Incrementation of the Vector3D by p
    \param p : vector to add to the current vector
*/
  void operator +=(const Vector3D &p);
/**
    \brief Comparison of two vectors, return true if equal
    \param p : vector to compare to the current vector
*/
  bool operator ==(const Vector3D &V1) { return (V1.pt[0]==pt[0] && V1.pt[1]==pt[1] && V1.pt[2]==pt[2] && V1.pt[3]==pt[3]); };
  friend istream& operator>>(istream& f,Vector3D &p);
  friend ostream& operator<<(ostream& f,const Vector3D &p);
  friend const Vector3D operator *(double,const Vector3D);
/**
    \brief Return the scalar product of two vectors
    \param v1 : first vector
    \param v2 : second vector
*/
  friend const double operator *(const Vector3D v1,const Vector3D v2);
  friend const Vector3D operator +(const Vector3D,const Vector3D);
  friend const Vector3D operator -(const Vector3D,const Vector3D);
  friend const Vector3D operator -(const Vector3D);
/**
    \brief Return the cross product of two vectors
    \param v1 : first vector
    \param v2 : second vector
*/
  friend const Vector3D operator ^(const Vector3D,const Vector3D);
  inline const double operator[](const int i) const { return pt[i]; };
  inline void setPoint(bool v) { pt[3]=(double)v; };
};

const Vector3D operator *(double,const Vector3D);
const double operator *(const Vector3D,const Vector3D);
const Vector3D operator +(const Vector3D,const Vector3D);
const Vector3D operator -(const Vector3D,const Vector3D);
const Vector3D operator -(const Vector3D);
const Vector3D operator ^(const Vector3D,const Vector3D);


#endif /* VECTOR3D_H_ */
