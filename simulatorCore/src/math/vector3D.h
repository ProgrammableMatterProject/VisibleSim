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
class Vector3D {
    float _pt[4]; //!< x,y,z,w in a table (w=0 for a vector, w=1 for a point)
public :

/**
   \brief Constructor, initialize the vector to 0
*/
    Vector3D():_pt{0,0,0,0} {};
/**
   \brief Constructor, initialize the vector to (x,y,z,w)
   \param x : x coordinate of the vector
   \param y : y coordinate of the vector
   \param z : z coordinate of the vector
   \param w : w=0 for a vector (default), w=1 for a point
*/
   // Vector3D(float x,float y,float z,float w=0.0):_pt{x,y,z,w} {};
    Vector3D(double x,double y,double z,double w=0.0):_pt{float(x),float(y),float(z),float(w)} {};
/**
   \brief Set method, initialize the vector to (x,y,z,w)
   \param x : x coordinate of the vector
   \param y : y coordinate of the vector
   \param z : z coordinate of the vector
   \param w : w=0 for a vector (default), w=1 for a point
*/
    inline void set(float x,float y,float z,float w=0.0) { _pt[0]=x; _pt[1]=y; _pt[2]=z; _pt[3]=w; };
/**
   \brief Set method, initialize the vector from an array of float
   \param tab : tab of coordinates
   \param s : size of tab
*/
    void set(const float *tab,short s,float extra=0.0f);
    void set(int i,double v) { _pt[i]=float(v); }
    void setMin(double x,double y,double z) { if (x<_pt[0]) _pt[0]=x; if (y<_pt[1]) _pt[1]=y; if (z<_pt[2]) _pt[2]=z; };
    void setMax(double x,double y,double z) { if (x>_pt[0]) _pt[0]=x; if (y>_pt[1]) _pt[1]=y; if (z>_pt[2]) _pt[2]=z; };

    /**
       \brief Return the scalar product of current vector with another
    */    
    const Vector3D dot(const Vector3D p) const;
    
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
   \brief Decrementation of the Vector3D by p
   \param p : vector to substrate to the current vector
*/
    void operator -=(const Vector3D &p);
/**
   \brief Multiplication of the Vector3D by p
   \param p : vector to multiply component by component to the current vector
*/
    void operator *=(const Vector3D &p);
    void operator /=(const Vector3D &p);
/**
   \brief Comparison of two vectors, return true if equal
   \param p : vector to compare to the current vector
*/
    bool operator ==(const Vector3D &V1) { return (V1._pt[0]==_pt[0] && V1._pt[1]==_pt[1] && V1._pt[2]==_pt[2] && V1._pt[3]==_pt[3]); };
    bool operator !=(const Vector3D &V1) { return not operator==(V1); }
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
    inline const float operator[](const int i) const { return _pt[i]; };
    inline void setPoint(bool v) { _pt[3]=(float)v; };
/**
    \brief Returns a boolean indicating if one of the vector's component is zero
    \return true if one of the components is zero, false otherwise
*/
    inline bool isZero() const { return _pt[0] == 0 || _pt[1] == 0 || _pt[2] == 0; };
		
		inline bool isInBox(const Vector3D &A, const Vector3D &B) {
			return (_pt[0]>=A._pt[0] && _pt[0]<=B._pt[0] &&
							_pt[1]>=A._pt[1] && _pt[1]<=B._pt[1] &&
							_pt[2]>=A._pt[2] && _pt[2]<=B._pt[2]);
		}

	inline float *getPtr() { return _pt; }
};

const Vector3D operator *(double,const Vector3D);
const double operator *(const Vector3D,const Vector3D);
const Vector3D operator +(const Vector3D,const Vector3D);
const Vector3D operator -(const Vector3D,const Vector3D);
const Vector3D operator -(const Vector3D);
const Vector3D operator ^(const Vector3D,const Vector3D);


#endif /* VECTOR3D_H_ */
