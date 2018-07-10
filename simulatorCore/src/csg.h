/*
 * csg.h
 *
 *  Created on: 4 september 2015
 *      Author: Thadeu
 */

#ifndef CSG_H_
#define CSG_H_

#include <vector>
#include "color.h"
#include "matrix44.h"

class CSGTreeStats
{
public:
    int leaf;
    int internal;
    int depth;
    CSGTreeStats() : leaf(0), internal(0), depth(0) {};
};

class BoundingBox {
public:
    Vector3D P0,P1;
    BoundingBox() {};
    BoundingBox(const Vector3D &d0,const Vector3D &d1) : P0(d0),P1(d1) {};
};

const BoundingBox operator |(const BoundingBox,const BoundingBox);

class CSGNode
{
protected:
    vector<CSGNode*> children;
public:

    CSGNode() {};
    virtual ~CSGNode() {};

    void addChild(CSGNode *n);
    void getStats(CSGTreeStats &stats, int depth);

    virtual void toString() const = 0;
    virtual bool isInside(const Vector3D &p, Color &color) const = 0;
    virtual bool isInBorder(const Vector3D &p, Color &color, double border) const = 0;
    virtual void boundingBox(BoundingBox &bb) = 0;
    virtual void glDraw() ;
};

/******************************************************************/

class CSGCube : public CSGNode
{
private:
    double size_x;
    double size_y;
    double size_z;
    bool center;

public:
    CSGCube (double _size_x, double _size_y, double _size_z);
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
    void glDraw() override;
};

class CSGSphere : public CSGNode
{
private:
    double radius;
public:
    CSGSphere (double _radius) : radius(_radius){};
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};

class CSGCylinder : public CSGNode
{
private:
    double height, radius;
    bool center;
public:
    CSGCylinder (double h, double r);
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};
/******************************************************************/

class CSGUnion : public CSGNode
{
public:
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};

class CSGDifference : public CSGNode
{
public:
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
    void glDraw() override;    
};

class CSGIntersection : public CSGNode
{
public:
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};
/******************************************************************/

class CSGTranslate : public CSGNode
{
private:
    Vector3D translate;

public:
    CSGTranslate(float x, float y, float z) { translate.set(x,y,z); };
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};

class CSGRotate : public CSGNode
{
private:
    Vector3D vec;
    Matrix rotate, rotate_1;

public:
    CSGRotate(float x, float y, float z);
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};

class CSGScale : public CSGNode
{
private:
    Vector3D scale;

public:
    CSGScale(float x, float y, float z) { scale.set(x,y,z); };
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};

/******************************************************************/
class CSGColor : public CSGNode
{
private:
    Color color;
public:
    CSGColor(int c1, int c2, int c3) { color.set(c1/255.,c2/255.,c3/255.); };
    void toString() const;
    bool isInside(const Vector3D &point, Color &color) const;
    bool isInBorder(const Vector3D &p, Color &color, double border) const;
    void boundingBox(BoundingBox &bb);
};


#endif /* CSG_H_ */


