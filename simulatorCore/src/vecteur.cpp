/*!
 * \file vecteur.cpp
 * \brief 3D homogeneous vector
 * \date 29/01/2012
 * \author Benoît Piranda
 */

#include "vecteur.h"

// lecture d'un vecteur dans un flux
istream& operator>>(istream& f,Vecteur&p)
{ f >> p.pt[0] >> p.pt[1] >> p.pt[2];
  return f;
}

// écriture d'un vecteur dans un flux
ostream& operator<<(ostream& f,const Vecteur&p)
{ f << "(" << p.pt[0] << "," << p.pt[1] << "," << p.pt[2] << "," << p.pt[3] << ")";
  return f;
}

const Vecteur operator *(double v,const Vecteur p)
{ Vecteur r;
  r.pt[0] = p.pt[0]*v;
  r.pt[1] = p.pt[1]*v;
  r.pt[2] = p.pt[2]*v;
  r.pt[3] = 0.;
  return r;
}

const double operator *(const Vecteur p1, const Vecteur p2)
{ return p1.pt[0]*p2.pt[0] + p1.pt[1]*p2.pt[1] + p1.pt[2]*p2.pt[2];
}

void Vecteur::operator +=(const Vecteur &p)
{ pt[0]+=p.pt[0];
  pt[1]+=p.pt[1];
  pt[2]+=p.pt[2];
  pt[3]+=p.pt[3];
}

const Vecteur operator +(const Vecteur p1, const Vecteur p2)
{ Vecteur r;
  r.pt[0] = p1.pt[0]+p2.pt[0];
  r.pt[1] = p1.pt[1]+p2.pt[1];
  r.pt[2] = p1.pt[2]+p2.pt[2];
  r.pt[3] = p1.pt[3]+p2.pt[3];
  return r;
}

const Vecteur operator -(const Vecteur p1, const Vecteur p2)
{ Vecteur r;
  r.pt[0] = p1.pt[0]-p2.pt[0];
  r.pt[1] = p1.pt[1]-p2.pt[1];
  r.pt[2] = p1.pt[2]-p2.pt[2];
  r.pt[3] = p1.pt[3]-p2.pt[3];
  return r;
}

const Vecteur operator -(const Vecteur p1)
{ Vecteur r;
  r.pt[0] = -p1.pt[0];
  r.pt[1] = -p1.pt[1];
  r.pt[2] = -p1.pt[2];
  r.pt[3] = 0.;
  return r;
}

const Vecteur operator ^(const Vecteur p1, const Vecteur p2)
{ Vecteur r;
  r.pt[0] = p1.pt[1]*p2.pt[2] - p1.pt[2]*p2.pt[1];
  r.pt[1] = p1.pt[2]*p2.pt[0] - p1.pt[0]*p2.pt[2];
  r.pt[2] = p1.pt[0]*p2.pt[1] - p1.pt[1]*p2.pt[0];
  r.pt[3] = 0.;
  return r;
}

const Vecteur Vecteur::normer() const
{ Vecteur r;
  double d=1.0/sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);

  r = d*(*this);
  return r;
}

double Vecteur::norme()
{ return sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
}

double Vecteur::norme2()
{ return pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2];
}

void Vecteur::normer_interne()
{ double d=1.0/sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
  pt[0]*=d;
  pt[1]*=d;
  pt[2]*=d;
  pt[3]=0;
}

void Vecteur::setLength(double l)
{ double d=l/sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
  pt[0]*=d;
  pt[1]*=d;
  pt[2]*=d;
  pt[3]=0;
}
