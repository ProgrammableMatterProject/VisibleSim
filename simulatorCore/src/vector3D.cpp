
/*!
 * \file Vector3D.cpp
 * \brief 3D homogeneous vector
 * \date 29/01/2012
 * \author Benoît Piranda
 */

#include "vector3D.h"

// lecture d'un Vector3D dans un flux
istream& operator>>(istream& f,Vector3D&p)
{ f >> p.pt[0] >> p.pt[1] >> p.pt[2];
  return f;
}

// écriture d'un Vector3D dans un flux
ostream& operator<<(ostream& f,const Vector3D&p)
{ f << "(" << p.pt[0] << "," << p.pt[1] << "," << p.pt[2] << "," << p.pt[3] << ")";
  return f;
}

const Vector3D Vector3D::dot(const Vector3D p) const{
    return Vector3D(pt[0] * p.pt[0],
                    pt[1] * p.pt[1],
                    pt[2] * p.pt[2]);
}

const Vector3D operator *(double v,const Vector3D p)
{ Vector3D r;
  r.pt[0] = p.pt[0]*v;
  r.pt[1] = p.pt[1]*v;
  r.pt[2] = p.pt[2]*v;
  r.pt[3] = 0.;
  return r;
}

const double operator *(const Vector3D p1, const Vector3D p2)
{ return p1.pt[0]*p2.pt[0] + p1.pt[1]*p2.pt[1] + p1.pt[2]*p2.pt[2];
}

void Vector3D::operator +=(const Vector3D &p)
{ pt[0]+=p.pt[0];
  pt[1]+=p.pt[1];
  pt[2]+=p.pt[2];
  pt[3]+=p.pt[3];
}

const Vector3D operator +(const Vector3D p1, const Vector3D p2)
{ Vector3D r;
  r.pt[0] = p1.pt[0]+p2.pt[0];
  r.pt[1] = p1.pt[1]+p2.pt[1];
  r.pt[2] = p1.pt[2]+p2.pt[2];
  r.pt[3] = p1.pt[3]+p2.pt[3];
  return r;
}

const Vector3D operator -(const Vector3D p1, const Vector3D p2)
{ Vector3D r;
  r.pt[0] = p1.pt[0]-p2.pt[0];
  r.pt[1] = p1.pt[1]-p2.pt[1];
  r.pt[2] = p1.pt[2]-p2.pt[2];
  r.pt[3] = p1.pt[3]-p2.pt[3];
  return r;
}

const Vector3D operator -(const Vector3D p1)
{ Vector3D r;
  r.pt[0] = -p1.pt[0];
  r.pt[1] = -p1.pt[1];
  r.pt[2] = -p1.pt[2];
  r.pt[3] = 0.;
  return r;
}

const Vector3D operator ^(const Vector3D p1, const Vector3D p2)
{ Vector3D r;
  r.pt[0] = p1.pt[1]*p2.pt[2] - p1.pt[2]*p2.pt[1];
  r.pt[1] = p1.pt[2]*p2.pt[0] - p1.pt[0]*p2.pt[2];
  r.pt[2] = p1.pt[0]*p2.pt[1] - p1.pt[1]*p2.pt[0];
  r.pt[3] = 0.;
  return r;
}

const Vector3D Vector3D::normer() const
{ Vector3D r;
  double d=1.0/sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);

  r = d*(*this);
  return r;
}

double Vector3D::norme() const
{ return sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
}

double Vector3D::norme2() const
{ return pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2];
}

void Vector3D::normer_interne()
{ double d=1.0/sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
  pt[0]*=d;
  pt[1]*=d;
  pt[2]*=d;
  pt[3]=0;
}

void Vector3D::setLength(double l)
{ double d=l/sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
  pt[0]*=d;
  pt[1]*=d;
  pt[2]*=d;
  pt[3]=0;
}

void Vector3D::set(const float *tab,short s,float extra) {
    short i;
    for (i=0; i<s; i++) {
        pt[i]=tab[i];
    }
    for (i=s; i<4; i++) {
        pt[i]=extra;
    }
};
