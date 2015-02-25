/*
 * Vecteur.h
 *
 *  Created on: 29 janv. 2012
 *
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

class Vecteur
{ public :
  double pt[4];

  Vecteur() { memset(pt,0,4*sizeof(double)); };
  Vecteur(double x,double y,double z,double w=0.0) { pt[0]=x; pt[1]=y; pt[2]=z; pt[3]=w; };
  inline void set(double x,double y,double z,double w=0.0) { pt[0]=x; pt[1]=y; pt[2]=z; pt[3]=w; };
  void setMin(double x,double y,double z) { if (x<pt[0]) pt[0]=x; if (y<pt[1]) pt[1]=y; if (z<pt[2]) pt[2]=z; };
  void setMax(double x,double y,double z) { if (x>pt[0]) pt[0]=x; if (y>pt[1]) pt[1]=y; if (z>pt[2]) pt[2]=z; };
  const Vecteur normer() const;
  void normer_interne();
  void setLength(double l);
  double norme();
  double norme2();
  void operator +=(const Vecteur &);
  bool operator ==(const Vecteur &V1) { return (V1.pt[0]==pt[0] && V1.pt[1]==pt[1] && V1.pt[2]==pt[2] && V1.pt[3]==pt[3]); };
  friend istream& operator>>(istream& f,Vecteur &p);
  friend ostream& operator<<(ostream& f,const Vecteur &p);
  friend const Vecteur operator *(double,const Vecteur);
  friend const double operator *(const Vecteur,const Vecteur);
  friend const Vecteur operator +(const Vecteur,const Vecteur);
  friend const Vecteur operator -(const Vecteur,const Vecteur);
  friend const Vecteur operator -(const Vecteur);
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
