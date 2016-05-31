/*
 * csg.h
 *
 *  Created on: 4 september 2015
 *      Author: Thadeu
 */

#ifndef CSG_H_
#define CSG_H_

#include <vector>
#include "vecteur.h"

enum class node_t {bool_op, shape, transformation, color, end};

class Shape3D
{
public:
    virtual bool isInside(Vecteur basePos, Vecteur p) = 0;
    virtual ~Shape3D() {};
    virtual Shape3D* clone() = 0;
};

class Cube : public Shape3D
{
public:
    double size_x;
    double size_y;
    double size_z;

    Cube (double _size_x, double _size_y, double _size_z) : size_x(_size_x), size_y(_size_y), size_z(_size_z) {};
    virtual ~Cube() {};

    void print();
    bool isInside(Vecteur basePos, Vecteur p);

    virtual Shape3D* clone() { return new Cube(*this); }
};

class Sphere : public Shape3D
{
public:
    double radius;
    Sphere (double _radius) : radius(_radius){};
    virtual ~Sphere() {};

    void print();
    bool isInside(Vecteur basePos, Vecteur p);

    virtual Shape3D* clone() { return new Sphere(*this); };
};

class Cylinder : public Shape3D
{
public:
    double h; // height
    double r; // radius
    Cylinder (double _h, double _r) : h(_h), r(_r) {};
    virtual ~Cylinder() {};

    void print();
    bool isInside(Vecteur basePos, Vecteur p);

    virtual Shape3D* clone() { return new Cylinder(*this); };
};

class BoolOperator
{
public:
    enum class bool_operator_t {bool_union, bool_difference, bool_intersection};
    bool_operator_t my_type;

    BoolOperator(bool_operator_t bool_type) : my_type(bool_type) {};

    void print();
};

class Transformation
{
public:
    enum class transformation_t {translate, rotate, scale};
    transformation_t my_type;
    double x, y, z;

    Transformation(transformation_t transformation_type, double _x, double _y, double _z) : my_type(transformation_type), x(_x), y(_y), z(_z) {};

    void print();
};


class NodeColor
{
public:
    float r, g, b;
    string oi;
    NodeColor(int c1, int c2, int c3, string o) : r(c1), g(c2), b(c3), oi(o) {};
    void print() { cout << "Color: " <<  r << ' ' << g << ' ' << b << ' ' << oi << endl; };
};

class CsgNode
{
    void *value;
    node_t type;
public:
    vector<CsgNode> vchildren;

    CsgNode();
    CsgNode(node_t _type, void *obj) : value(obj), type(_type) {};
    CsgNode(const CsgNode &source);
    ~CsgNode();

    CsgNode& operator=(const CsgNode &other);
    void addChild(CsgNode &n);
    void printTree();
    void* getValue() { return value; };
    node_t getType() { return type; };
};

#endif /* CSG_H_ */


