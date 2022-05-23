/*!
 * \file Vector3D.cpp
 * \brief 3D homogeneous vector
 * \date 29/01/2012
 * \author Benoît Piranda
 */

#include "vector3D.h"

// lecture d'un Vector3D dans un flux
istream &operator>>(istream &f, Vector3D &p) {
    f >> p._pt[0] >> p._pt[1] >> p._pt[2];
    return f;
}

// écriture d'un Vector3D dans un flux
ostream &operator<<(ostream &f, const Vector3D &p) {
    f << "(" << p._pt[0] << "," << p._pt[1] << "," << p._pt[2] << "," << p._pt[3] << ")";
    return f;
}

const Vector3D Vector3D::dot(const Vector3D p) const {
    return Vector3D(_pt[0] * p._pt[0],
                    _pt[1] * p._pt[1],
                    _pt[2] * p._pt[2],
                    1.0);
}

const Vector3D operator*(double v, const Vector3D p) {
    Vector3D r;
    r._pt[0] = p._pt[0] * v;
    r._pt[1] = p._pt[1] * v;
    r._pt[2] = p._pt[2] * v;
    r._pt[3] = 0;
    return r;
}

const double operator*(const Vector3D p1, const Vector3D p2) {
    return p1._pt[0] * p2._pt[0] + p1._pt[1] * p2._pt[1] + p1._pt[2] * p2._pt[2];
}

void Vector3D::operator+=(const Vector3D &p) {
    _pt[0] += p._pt[0];
    _pt[1] += p._pt[1];
    _pt[2] += p._pt[2];
    _pt[3] += p._pt[3];
}

void Vector3D::operator-=(const Vector3D &p) {
    _pt[0] -= p._pt[0];
    _pt[1] -= p._pt[1];
    _pt[2] -= p._pt[2];
    _pt[3] -= p._pt[3];
}

void Vector3D::operator*=(const Vector3D &p) {
    _pt[0] *= p._pt[0];
    _pt[1] *= p._pt[1];
    _pt[2] *= p._pt[2];
    _pt[3] *= p._pt[3];
}

void Vector3D::operator/=(const Vector3D &p) {
    _pt[0] /= p._pt[0];
    _pt[1] /= p._pt[1];
    _pt[2] /= p._pt[2];
    _pt[3] *= p._pt[3]; //Jad: Before my commit it was *=
}


const Vector3D operator+(const Vector3D p1, const Vector3D p2) {
    Vector3D r;
    r._pt[0] = p1._pt[0] + p2._pt[0];
    r._pt[1] = p1._pt[1] + p2._pt[1];
    r._pt[2] = p1._pt[2] + p2._pt[2];
    r._pt[3] = p1._pt[3] + p2._pt[3];
    return r;
}

const Vector3D operator-(const Vector3D p1, const Vector3D p2) {
    Vector3D r;
    r._pt[0] = p1._pt[0] - p2._pt[0];
    r._pt[1] = p1._pt[1] - p2._pt[1];
    r._pt[2] = p1._pt[2] - p2._pt[2];
    r._pt[3] = p1._pt[3] - p2._pt[3];
    return r;
}

const Vector3D operator-(const Vector3D p1) {
    Vector3D r;
    r._pt[0] = -p1._pt[0];
    r._pt[1] = -p1._pt[1];
    r._pt[2] = -p1._pt[2];
    r._pt[3] = 0.;
    return r;
}

const Vector3D operator^(const Vector3D p1, const Vector3D p2) {
    Vector3D r;
    r._pt[0] = p1._pt[1] * p2._pt[2] - p1._pt[2] * p2._pt[1];
    r._pt[1] = p1._pt[2] * p2._pt[0] - p1._pt[0] * p2._pt[2];
    r._pt[2] = p1._pt[0] * p2._pt[1] - p1._pt[1] * p2._pt[0];
    r._pt[3] = 0.;
    return r;
}

const Vector3D Vector3D::normer() const {
    Vector3D r;
    double d = 1.0 / sqrt(_pt[0] * _pt[0] + _pt[1] * _pt[1] + _pt[2] * _pt[2]);

    r = d * (*this);
    return r;
}

double Vector3D::norme() const {
    return sqrt(_pt[0] * _pt[0] + _pt[1] * _pt[1] + _pt[2] * _pt[2]);
}

double Vector3D::norme2() const {
    return _pt[0] * _pt[0] + _pt[1] * _pt[1] + _pt[2] * _pt[2];
}

void Vector3D::normer_interne() {
    double d = 1.0 / sqrt(_pt[0] * _pt[0] + _pt[1] * _pt[1] + _pt[2] * _pt[2]);
    _pt[0] *= d;
    _pt[1] *= d;
    _pt[2] *= d;
    _pt[3] = 0;
}

void Vector3D::setLength(double l) {
    double d = l / sqrt(_pt[0] * _pt[0] + _pt[1] * _pt[1] + _pt[2] * _pt[2]);
    _pt[0] *= d;
    _pt[1] *= d;
    _pt[2] *= d;
    _pt[3] = 0;
}

void Vector3D::set(const float *tab, short s, float extra) {
    short i;
    for (i = 0; i < s; i++) {
        _pt[i] = tab[i];
    }
    for (i = s; i < 4; i++) {
        _pt[i] = extra;
    }
}
