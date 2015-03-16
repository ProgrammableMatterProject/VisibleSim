/*!
 * \file vecteur.h
 * \brief 3D homogeneous vector
 * \date 29/01/2012
 * \author Benoît Piranda
 */

#ifndef VECTEUR_H_
#define VECTEUR_H_

#include <iostream>
#include <fstream>
#include <cmath>
#ifndef _WIN
#include <memory.h>
#endif

#ifndef M_PI
#define M_PI	3.1415926535897932384626433832795
#endif

using namespace std;

/**
 * \class Vecteur vecteur.h
*/
class Vecteur
{ public :
  double pt[4]; //!< x,y,z,w in a table (w=0 for a vector, w=1 for a point)

/**
    \brief Constructor, initialize the vector to 0
*/
  Vecteur() { memset(pt,0,4*sizeof(double)); };
/**
    \brief Constructor, initialize the vector to (x,y,z,w)
    \param x : x coordinate of the vector
    \param y : y coordinate of the vector
    \param z : z coordinate of the vector
    \param w : w=0 for a vector (default), w=1 for a point
*/
  Vecteur(double x,double y,double z,double w=0.0) { pt[0]=x; pt[1]=y; pt[2]=z; pt[3]=w; };
/**
    \brief Set method, initialize the vector to (x,y,z,w)
    \param x : x coordinate of the vector
    \param y : y coordinate of the vector
    \param z : z coordinate of the vector
    \param w : w=0 for a vector (default), w=1 for a point
*/
  inline void set(double x,double y,double z,double w=0.0) { pt[0]=x; pt[1]=y; pt[2]=z; pt[3]=w; };
  void setMin(double x,double y,double z) { if (x<pt[0]) pt[0]=x; if (y<pt[1]) pt[1]=y; if (z<pt[2]) pt[2]=z; };
  void setMax(double x,double y,double z) { if (x>pt[0]) pt[0]=x; if (y>pt[1]) pt[1]=y; if (z>pt[2]) pt[2]=z; };
/**
    \brief Return a normalized copy of the vector
*/
  const Vecteur normer() const;
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
    \brief Incrementation of the vecteur by p
    \param p : vector to add to the current vector
*/
  void operator +=(const Vecteur &p);
/**
    \brief Comparison of two vectors, return true if equal
    \param p : vector to compare to the current vector
*/
  bool operator ==(const Vecteur &V1) { return (V1.pt[0]==pt[0] && V1.pt[1]==pt[1] && V1.pt[2]==pt[2] && V1.pt[3]==pt[3]); };
  friend istream& operator>>(istream& f,Vecteur &p);
  friend ostream& operator<<(ostream& f,const Vecteur &p);
  friend const Vecteur operator *(double,const Vecteur);
/**
    \brief Return the scalar product of two vectors
    \param v1 : first vector
    \param v2 : second vector
*/
  friend const double operator *(const Vecteur v1,const Vecteur v2);
  friend const Vecteur operator +(const Vecteur,const Vecteur);
  friend const Vecteur operator -(const Vecteur,const Vecteur);
  friend const Vecteur operator -(const Vecteur);
/**
    \brief Return the cross product of two vectors
    \param v1 : first vector
    \param v2 : second vector
*/
  friend const Vecteur operator ^(const Vecteur,const Vecteur);
  inline const double operator[](const int i) const { return pt[i]; };
  inline void setPoint(bool v) { pt[3]=(double)v; };
};

const Vecteur operator *(double,const Vecteur);
const double operator *(const Vecteur,const Vecteur);
const Vecteur operator +(const Vecteur,const Vecteur);
const Vecteur operator -(const Vecteur,const Vecteur);
const Vecteur operator -(const Vecteur);
const Vecteur operator ^(const Vecteur,const Vecteur);


#endif /* VECTEUR_H_ */
