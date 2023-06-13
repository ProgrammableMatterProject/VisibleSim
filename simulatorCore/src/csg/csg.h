/*
 * csg.h
 *
 *  Created on: 4 september 2015
 *      Author: Thadeu
 */

#ifndef CSG_H_
#define CSG_H_

#include <vector>
#include "../utils/color.h"
#include "../math/matrix44.h"
#include "../utils/trace.h"

class CSGTreeStats {
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

class CSGNode {
protected:
    vector<CSGNode*> children;
public:

    CSGNode() {};
    virtual ~CSGNode() {};

    void addChild(CSGNode *n);
    void getStats(CSGTreeStats &stats, int depth);

    virtual void toString() const = 0;
    virtual string toCode() const=0;
    virtual bool isInside(const Vector3D &p, Color &color) const = 0;
    virtual bool isInBorder(const Vector3D &p, Color &color, double border) const = 0;
    virtual void boundingBox(BoundingBox &bb) = 0;
    virtual void glDraw() ;
};

/******************************************************************/

class CSGCube : public CSGNode {
private:
    double size_x;
    double size_y;
    double size_z;
    bool center;
public:
    CSGCube (const Vector3D &p_vec,bool p_center);
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
    void glDraw() override;
};

class CSGSphere : public CSGNode {
private:
    double radius;
public:
    explicit CSGSphere (double _radius) : radius(_radius){};
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

class CSGCylinder : public CSGNode {
private:
    double height, radius;
    bool center;
public:
    CSGCylinder(double h, double r, bool p_center);
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

class CSGCone : public CSGNode {
private:
    double height, bottomRadius, topRadius;
    bool center;
public:
    CSGCone(double p_height, double p_bottomRadius, double p_topRadius, bool p_center);
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

class CSGTorus : public CSGNode {
private:
    double radius1, radius2;
public:
    CSGTorus(double p_r1, double p_r2);
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

/******************************************************************/

class CSGUnion : public CSGNode {
public:
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

class CSGDifference : public CSGNode {
public:
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
    void glDraw() override;
};

class CSGIntersection : public CSGNode {
public:
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};
/******************************************************************/

class CSGTranslate : public CSGNode {
private:
    Vector3D translate;
public:
    explicit CSGTranslate(const Vector3D &p_vect):translate(p_vect) {};
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

class CSGRotate : public CSGNode {
private:
    Vector3D vec;
    Matrix rotate, rotate_1;
public:
    explicit CSGRotate(const Vector3D &p_vec);
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

class CSGScale : public CSGNode {
private:
    Vector3D scale;
public:
    explicit CSGScale(const Vector3D &p_scale):scale(p_scale) {};
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

/******************************************************************/
class CSGColor : public CSGNode {
private:
    Color color;
public:
    explicit CSGColor(const Vector3D& p_vec):color(p_vec) {};
    void toString() const override;
    string toCode() const override;
    bool isInside(const Vector3D &point, Color &color) const override;
    bool isInBorder(const Vector3D &p, Color &color, double border) const override;
    void boundingBox(BoundingBox &bb) override;
};

#endif /* CSG_H_ */
